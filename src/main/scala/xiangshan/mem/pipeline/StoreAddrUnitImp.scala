/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.datapath.NewPipelineConnectPipe
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp, Pbmt}
import xiangshan.cache.{DcacheStoreRequestIO, DCacheStoreIO, MemoryOpConstants, HasDCacheParameters, StorePrefetchReq}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO}
import xiangshan.mem.ReplayCauseNo._
import xiangshan.mem.Bundles._

class StoreAddrUnitIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO {
  // to
  val toBackend = new Bundle() {
    val issue     = ValidIO(new LsPipelineBundle)
    val iqFeedback  = ValidIO(new RSFeedback)
  }
  val toLsq = new Bundle() {
    val addrRelatedUpdate = ValidIO(new LsPipelineBundle)
    val excpRelatedUpdate = ValidIO(new LsPipelineBundle)
    val maskOut = ValidIO(new StoreMaskBundle)
  }
  val toLdu = ValidIO(new StoreNukeQueryBundle)
  val toPrefetch = new LsPrefetchTrainIO
  val toMisalignBuf = new Bundle() {
    val enq = ValidIO(new LsPipelineBundle)
    val wb  = OptionWrapper(params.hasMisalignExe, ValidIO(new LsPipelineBundle))
  }
  // debug
  val debugLsInfo   = Output(new DebugLsInfoBundle)
}

class StoreAddrUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
{
  private val lgSelectGroupSize = log2Ceil(RollbackGroupSize)
  private val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1
  private val TotalDelayCycles  = TotalSelectCycles - 2

  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }
  val unitParams = params

  io.suggestName("none")
  override lazy val io = IO(new StoreAddrUnitIO).suggestName("io")

  private val toTlb = io.toTlb
  private val toDCache = io.toDCache
  private val fromTlb = io.fromTlb
  private val fromDCache = io.fromDCache
  private val toLsq = io.toLsq
  private val toLdu = io.toLdu
  private val toPrefetch = io.toPrefetch
  private val toMisalignBuf = io.toMisalignBuf
  private val toBackend = io.toBackend
  private val debugLsInfo = io.debugLsInfo

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------

  // make dev happy
  val s0FuOpType = Mux(s0Out.bits.isVector, s0Out.bits.alignedType(1, 0), s0Out.bits.uop.fuOpType(1, 0))
  val s0AddrAligned = LookupTree(s0FuOpType, List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (s0Out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (s0Out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (s0Out.bits.vaddr(2,0) === 0.U)  //d
  ))

  // if vector store sends 128-bit requests, its address must be 128-aligned
  XSError(s0Out.valid && s0Out.bits.isVector && s0Out.bits.vaddr(3, 0) =/= 0.U && s0Out.bits.alignedType(2),
         "unit stride 128 bit element is not aligned!")
  // make dev happy
  val s0VectorMisaligned = s0ArbOut.bits.uop.exceptionVec(storeAddrMisaligned)
  val s0NonPrefetchMisAligned = (!s0AddrAligned || s0VectorMisaligned && s0ArbOut.bits.vecActive)
  s0Out.bits.uop.exceptionVec(storeAddrMisaligned) := !s0Out.bits.isPrefetch && s0NonPrefetchMisAligned

  // to tlb
  toTlb.req.bits.cmd          := TlbCmd.write
  toTlb.req.bits.memidx.is_ld := false.B
  toTlb.req.bits.memidx.is_st := true.B
  toTlb.req.bits.memidx.idx   := s0Out.bits.uop.sqIdx.value
  toTlb.req.bits.no_translate := false.B
  toTlb.req.bits.hyperinst    := LSUOpType.isHsv(s0Out.bits.uop.fuOpType)
  toTlb.req.bits.hlvx         := false.B
  toTlb.req.bits.pmp_addr     := DontCare
  toTlb.req_kill              := false.B

  // Dcache access here: not **real** dcache write
  // just read meta and tag in dcache, to find out the store will hit or miss

  // NOTE: The store request does not wait for the dcache to be ready.
  //       If the dcache is not ready at this time, the dcache is not queried.
  //       But, store prefetch request will always wait for dcache to be ready to make progress.
  toDCache.req.bits.cmd       := MemoryOpConstants.M_PFW
  toDCache.req.bits.instrtype := Mux(
                                    s0Out.bits.isPrefetch,
                                    DCACHE_PREFETCH_SOURCE.U,
                                    STORE_SOURCE.U
                                 )

  // to lsq
  toLsq.maskOut.valid      := s0Out.valid && (s0Out.bits.isIq || s0Out.bits.isVector)
  toLsq.maskOut.bits.mask  := s0Out.bits.mask
  toLsq.maskOut.bits.sqIdx := s0Out.bits.uop.sqIdx

  XSPerfAccumulate("s0VecIn", s0Out.fire && s0Out.bits.isVector)
  val s0AddrHighMatch = s0Out.bits.vaddr(VAddrBits-1, 12) === s0Out.bits.src(0)(VAddrBits-1, 12)
  XSPerfAccumulate("s0AddrSpecSuccess",  s0Out.fire && !s0Out.bits.isVector && s0AddrHighMatch)
  XSPerfAccumulate("s0AddrSpecFailed",   s0Out.fire && !s0Out.bits.isVector && !s0AddrHighMatch)
  XSPerfAccumulate("s0AddrSpecSuccessOnce", s0Out.fire && !s0Out.bits.isVector && s0AddrHighMatch && s0Out.bits.isFirstIssue)
  XSPerfAccumulate("s0AddrSpecFailedOnce", s0Out.fire && !s0Out.bits.isVector && !s0AddrHighMatch && s0Out.bits.isFirstIssue)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // TLB resp (send paddr to dcache)
  val s1MMIOCbo = s1In.bits.uop.fuOpType === LSUOpType.cbo_clean ||
                  s1In.bits.uop.fuOpType === LSUOpType.cbo_flush ||
                  s1In.bits.uop.fuOpType === LSUOpType.cbo_inval
  s1Kill := s1In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect) ||
            (fromTlb.bits.miss && !s1In.bits.isVector && !s1In.bits.isMisalignBuf)

  // to ldu for st-ld violation dectect request.
  toLdu.valid := s1In.valid && !fromTlb.bits.miss && !s1In.bits.isHWPrefetch && !s1In.bits.isMisalignBuf
  toLdu.bits.robIdx := s1In.bits.uop.robIdx
  toLdu.bits.paddr  := fromTlb.bits.paddr(0)
  toLdu.bits.mask   := s1In.bits.mask
  toLdu.bits.matchLine := s1In.bits.isVector && s1In.bits.is128bit

  // to backend
  toBackend.issue.valid := s1In.valid && !fromTlb.bits.miss && s1In.bits.isIq
  toBackend.issue.bits  := s1In.bits

  // to MisalignBuffer
  // make dev happy
  val s1MisalignBufCanGo = !s1In.bits.isHWPrefetch && !s1In.bits.isVector
  toMisalignBuf.enq.valid := s1In.valid && s1MisalignBufCanGo &&
                         GatedValidRegNext(io.fromCtrl.csrCtrl.hd_misalign_st_enable)
  toMisalignBuf.enq.bits  := s1Out.bits
  toMisalignBuf.enq.bits.miss := fromTlb.bits.miss

  // Send TLB feedback to store issue queue
  // Store feedback is generated in store_s1, sent to Iq in store_s2
  val s1IqFeedback = Wire(Valid(new RSFeedback))
  val s1IqFeedbackCanGo = s1IqFeedback.valid && !s1In.bits.isVector && !s1In.bits.isMisalignBuf
  s1IqFeedback.valid           := s1In.valid && s1In.bits.isIq
  s1IqFeedback.bits.hit        := fromTlb.bits.miss
  s1IqFeedback.bits.flushState := fromTlb.bits.ptwBack
  s1IqFeedback.bits.robIdx     := s1In.bits.uop.robIdx
  s1IqFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  s1IqFeedback.bits.sqIdx      := s1In.bits.uop.sqIdx
  s1IqFeedback.bits.lqIdx      := s1In.bits.uop.lqIdx
  s1IqFeedback.bits.dataInvalidSqIdx := DontCare

  // scalar store and scalar load nuke check, and also other purposes
  toLsq.addrRelatedUpdate.valid := s1In.valid && !s1In.bits.isHWPrefetch && !s1In.bits.isMisalignBuf
  toLsq.addrRelatedUpdate.bits  := s1Out.bits
  toLsq.addrRelatedUpdate.bits.miss := fromTlb.bits.miss

  // kill dcache write intent request when tlb miss or exception
  val s1Exception = ExceptionNO.partialSelect(s1Out.bits.uop.exceptionVec, params.exceptionOut).asUInt.orR
  toDCache.s1_kill  := fromTlb.bits.miss || s1Exception || s1MMIOCbo ||
                          s1In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  s1Out.bits.miss   := false.B
  s1Out.bits.mmio   := s1MMIOCbo
  s1Out.bits.atomic := s1MMIOCbo
  // exception generate
  s1Out.bits.uop.exceptionVec(storePageFault)      := fromTlb.bits.excp(0).pf.st &&
                                                      s1In.bits.vecActive
  s1Out.bits.uop.exceptionVec(storeAccessFault)    := fromTlb.bits.excp(0).af.st &&
                                                      s1In.bits.vecActive
  s1Out.bits.uop.exceptionVec(storeGuestPageFault) := fromTlb.bits.excp(0).gpf.st &&
                                                      s1In.bits.vecActive

  // make dev happy
  val s1StoreFaultException = s1Out.bits.uop.exceptionVec(storePageFault) ||
                              s1Out.bits.uop.exceptionVec(storeAccessFault) ||
                              s1Out.bits.uop.exceptionVec(storeGuestPageFault)
  when (!s1In.bits.isVector && RegNext(toTlb.req.bits.checkfullva) && s1StoreFaultException) {
    s1Out.bits.uop.exceptionVec(storeAddrMisaligned) := false.B
  }

  if (params.hasPrefetch) {
    toPrefetch.s1PrefetchSpec := s1Out.fire
  } else {
    toPrefetch.s1PrefetchSpec := false.B
  }

  debugLsInfo := DontCare
  debugLsInfo.s1_robIdx := s1In.bits.uop.robIdx.value
  debugLsInfo.s1_isTlbFirstMiss := fromTlb.valid &&
                                   fromTlb.bits.miss &&
                                   fromTlb.bits.debug.isFirstIssue &&
                                   !s1In.bits.isHWPrefetch

  XSPerfAccumulate("s1InValid",           s1In.valid)
  XSPerfAccumulate("s1InFire",            s1In.fire)
  XSPerfAccumulate("s1InFireFirstIssue",  s1In.fire && s1In.bits.isFirstIssue)
  XSPerfAccumulate("s1TlbMiss",           s1In.fire && s1Out.bits.tlbMiss)
  XSPerfAccumulate("s1TlbMissFirstIssue", s1In.fire && s1Out.bits.tlbMiss && s1In.bits.isFirstIssue)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // mmio check
  val s2MMIO = (s2In.bits.mmio || io.fromPmp.mmio || Pbmt.isUncache(RegNext(fromTlb.bits.pbmt(0)))) &&
                RegNext(s1IqFeedback.bits.hit)
  val s2Exception = ExceptionNO.partialSelect(s2Out.bits.uop.exceptionVec, params.exceptionOut).asUInt.orR
  s2Kill := ((s2MMIO && !s2Exception) && !s2In.bits.isVector) || s2In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  s2Out.bits.af := s2Out.bits.uop.exceptionVec(storeAccessFault)
  s2Out.bits.mmio := s2MMIO && !s2Exception
  s2Out.bits.atomic := s2In.bits.atomic || io.fromPmp.atomic
  s2Out.bits.feedbacked := toBackend.iqFeedback.bits.hit

  // make dev happy
  val s2StoreAccessFault = s2In.bits.uop.exceptionVec(storeAccessFault)
  val s2PmpRespFault = io.fromPmp.st
  val s2VectorMMIOFault = s2In.bits.isVector && io.fromPmp.mmio && RegNext(s1IqFeedback.bits.hit)
  s2Out.bits.uop.exceptionVec(storeAccessFault) := (s2StoreAccessFault || s2PmpRespFault || s2VectorMMIOFault) && s2In.bits.vecActive

  // kill dcache write intent request when mmio or exception
  toDCache.s2_kill := (s2MMIO || s2Exception || s2In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect))

  // toBackend
  // feedback tlb miss to RS in store_s2
  // make dev happy
  toBackend.iqFeedback.valid := GatedValidRegNext(s1IqFeedbackCanGo && !s1In.bits.uop.robIdx.needFlush(io.fromCtrl.redirect))
  toBackend.iqFeedback.bits  := RegEnable(s1IqFeedback.bits, s1IqFeedbackCanGo)

  //  to MisalignBuf
  if (params.hasMisalignExe) {
    toMisalignBuf.wb.get.valid := s2In.valid && s2In.bits.isMisalignBuf
    toMisalignBuf.wb.get.bits  := s2Out.bits
    toMisalignBuf.wb.get.bits.causeVec(tlbMiss) := s2Out.bits.tlbMiss
  }

  //  to lsq
  toLsq.excpRelatedUpdate.valid := s2In.valid
  toLsq.excpRelatedUpdate.bits  := s2Out.bits
  toLsq.excpRelatedUpdate.bits.af := s2Out.bits.af && !s2Kill
  toLsq.excpRelatedUpdate.bits.miss := fromDCache.resp.fire && fromDCache.resp.bits.miss

  // RegNext prefetch train for better timing
  // ** Now, prefetch train is valid at store s3 **
  val s2PrefetchTrainValid = s2In.valid && fromDCache.resp.fire && !s2Out.bits.mmio && !s2In.bits.tlbMiss && !s2In.bits.isHWPrefetch
  if (params.hasPrefetch) {
    toPrefetch.s2PrefetchSpec := s2PrefetchTrainValid
    toPrefetch.req.valid := RegNext(s2PrefetchTrainValid)
    toPrefetch.req.bits.fromLsPipelineBundle(s2In.bits, latch = true, enable = s2PrefetchTrainValid)
  } else {
    toPrefetch.s2PrefetchSpec := false.B
    toPrefetch.req.valid := false.B
    toPrefetch.req.bits := DontCare
  }
  toPrefetch.req.bits.miss := RegEnable(fromDCache.resp.bits.miss, s2PrefetchTrainValid)
  toPrefetch.req.bits.metaPrefetch := false.B
  toPrefetch.req.bits.metaAccess   := false.B

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // store write back

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage x
  // --------------------------------------------------------------------------------
  // delay TotalSelectCycles - 2 cycle(s)
  val pipelineRegs = Seq.fill(TotalDelayCycles)(Module(new NewPipelineConnectPipe(new LsPipelineBundle)))
  val wbPortRegs = DelayN(s3WBPort, TotalDelayCycles)

  val exuOut = Wire(DecoupledIO(new LsPipelineBundle))
  if (pipelineRegs.length > 0) {
    pipelineRegs.head.io.in <> s3Out
    pipelineRegs.head.io.isFlush := s3Out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
    pipelineRegs.head.io.rightOutFire := pipelineRegs.head.io.out.fire
    pipelineRegs.dropRight(1).zip(pipelineRegs.drop(1)).map {
      case (source, sink) =>
        val isFlush = source.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
        sink.io.in <> source.io.out
        sink.io.isFlush := isFlush
        sink.io.rightOutFire := sink.io.out.fire
    }
    exuOut <> pipelineRegs.last.io.out
  } else {
    exuOut <> s3Out
  }

  io.toIssue.zip(wbPortRegs).map {
    case (toIssue, select) =>
      toIssue.valid := exuOut.valid && select
      toIssue.bits  := exuOut.bits
      when (select) {
        exuOut.ready := toIssue.ready
      }
  }
}
