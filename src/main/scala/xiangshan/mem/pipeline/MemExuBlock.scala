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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.NewCSR._
import xiangshan.mem.Bundles._
import xiangshan.cache.{HasDCacheParameters, StorePrefetchReq}
import xiangshan.cache.mmu.{TlbReq, TlbResp, TlbCmd}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO}

class MemExuBlock(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasMemBlockParameters
{
  val stdUnits = memUnitParams.filter(_.isStoreDataUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )
  val staUnits = memUnitParams.filter(_.isStoreAddrUnit).map(
    params => LazyModule(new MemUnit(params).suggestName(params.name))
  )

  lazy val module = new MemExuBlockImp(this)
}

class BackendToMemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  val loadFastMatch     = Vec(LdExuCnt, Input(UInt(LdExuCnt.W)))
  val loadFastFuOpType  = Vec(LdExuCnt, Input(FuOpType()))
  val loadFastImm       = Vec(LdExuCnt, Input(UInt(12.W)))

  val issueLda  = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta  = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd  = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya  = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))
}

class MemExuBlockToBackendIO(implicit p: Parameters) extends MemBlockBundle {
  val stIssue = Vec(StAddrCnt, ValidIO(new MemExuInput))

  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))


  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val vstuIqFeedback= Vec(VstuCnt, new MemRSFeedbackIO(isVector = true))
  val vlduIqFeedback= Vec(VlduCnt, new MemRSFeedbackIO(isVector = true))
  val ldCancel = Vec(backendParams.LdExuCnt, new LoadCancelIO)

  def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }
}

class MemExuBlockIO(implicit p: Parameters) extends MemBlockBundle {
  // from
  val fromCtrl = new Bundle () {
    val hartId    = Input(UInt(hartIdLen.W))
    val redirect  = Flipped(ValidIO(new Redirect))
    val csrCtrl   = Flipped(new CustomCSRCtrlIO)
    val trigger   = Input(new CsrTriggerBundle)
  }

  val fromBackend = new BackendToMemExuBlockIO
  val fromPrefetch = new Bundle() {
    val store = Vec(StaCnt, Flipped(DecoupledIO(new StorePrefetchReq)))
  }
  val fromTlb    = Vec(MemAddrExtCnt, Flipped(DecoupledIO(new TlbResp(2))))
  val fromPmp    = Vec(MemAddrExtCnt, Flipped(new PMPRespBundle()))
  val fromDCache = Vec(MemAddrExtCnt, new DCacheLoadRespIO)
  val fromLoadMisalignBuf = Vec(MemAddrExtCnt, Flipped(Decoupled(new LsPipelineBundle)))
  val fromStoreMisalignBuf = Vec(MemAddrExtCnt, Flipped(Decoupled(new LsPipelineBundle)))
  val fromVectorLoad = Vec(VlduCnt, Flipped(DecoupledIO(new VecPipeBundle(isVStore = false))))
  val fromVectorStore = Vec(VstuCnt, Flipped(DecoupledIO(new VecPipeBundle(isVStore = true))))

  // to
  val toBackend   = new MemExuBlockToBackendIO
  val toDCache    = Vec(MemAddrExtCnt, new DCacheLoadReqIO)
  val toTlb       = Vec(MemAddrExtCnt, new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  })
  val toPrefetch = new Bundle() {
    val load  = Vec(LdExuCnt, ValidIO(new LsPipelineBundle()))
    val store = Vec(StAddrCnt, ValidIO(new LsPrefetchTrainBundle()))
  }
  val toLoadMisalignBuf = Vec(LdExuCnt, ValidIO(new LsPipelineBundle))
  val toStoreMisalignBuf = new Bundle() {
    val enq = Vec(StAddrCnt, ValidIO(new LsPipelineBundle))
    val wb  = ValidIO(new LsPipelineBundle)
  }
  val toVectorLoad = Vec(VlduCnt, DecoupledIO(new VecPipelineFeedbackIO(isVStore = false)))
  val toVectorStore = Vec(VlduCnt, DecoupledIO(new VecPipelineFeedbackIO(isVStore = true)))
  val toLsq = new Bundle() {
    val addrRelatedUpdate = Vec(StAddrCnt, ValidIO(new LsPipelineBundle))
    val excpRelatedUpdate = Vec(StAddrCnt, ValidIO(new LsPipelineBundle))
    val maskOut = Vec(StAddrCnt, ValidIO(new StoreMaskBundle))
  }
  val toLdu = Vec(StAddrCnt, ValidIO(new StoreNukeQueryBundle))
}

class MemExuBlockImp(wrapper: MemExuBlock) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasMemBlockParameters
{

  val io = IO(new MemExuBlockIO)

  private val toBackend = io.toBackend
  private val toDCache  = io.toDCache
  private val toTlb     = io.toTlb
  private val toStoreMisalignBuf = io.toStoreMisalignBuf
  private val toPrefetch = io.toPrefetch
  private val toVectorStore = io.toVectorStore

  private val fromCtrl     = io.fromCtrl
  private val fromBackend  = io.fromBackend
  private val fromPrefetch = io.fromPrefetch
  private val fromTlb      = io.fromTlb
  private val fromPmp      = io.fromPmp
  private val fromDCache   = io.fromDCache
  private val fromStoreMisalignBuf = io.fromStoreMisalignBuf
  private val fromVectorStore = io.fromVectorStore

  val stdUnitImps = wrapper.stdUnits.map(_.module)
  val staUnitImps = wrapper.staUnits.map(_.module)

  // stdunits
  stdUnitImps.zipWithIndex.foreach {
    case (impl: StoreDataUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.hartId   <> fromCtrl.hartId
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl
      impl.io.fromTlb    := DontCare
      impl.io.fromDCache := DontCare
      impl.io.fromPmp    := DontCare
      impl.io.toDCache.req.ready := false.B
      impl.io.toTlb.req.ready := false.B
    case _ =>
  }

  require(stdUnitImps.map(_.io.fromIssue).flatten.length == fromBackend.issueStd.length,
          "The number of issueStd does not match!")
  stdUnitImps.map(_.io.fromIssue).flatten.zip(fromBackend.issueStd).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits.fromMemExuInputBundle(source.bits)
      source.ready := sink.ready
  }
  require(stdUnitImps.map(_.io.toIssue).flatten.length == toBackend.writebackStd.length,
          "The number of writebackStd does not match!")
  toBackend.writebackStd.zip(stdUnitImps.map(_.io.toIssue).flatten).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready
  }

  // staunits
  require(staUnitImps.map(_.params.issueParams).flatten.filter(_.isIq).length == fromBackend.issueSta.length,
          "The number of issuSta does not match!")
  require(staUnitImps.map(_.params.issueParams).flatten.filter(_.isPrefetch).length == fromPrefetch.store.length,
          "The number of prefetch store does not match!")
  require(staUnitImps.map(_.params.issueParams).flatten.filter(_.isVectorStore).length == toVectorStore.length,
          "The number of vector store does not match!")

  private val toStTlb    = toTlb.drop(LduCnt).take(StaCnt)
  private val toStDCache = toDCache.drop(LduCnt).take(StaCnt)
  private val fromStTlb  = fromTlb.drop(LduCnt).take(StaCnt)
  private val fromStPmp  = fromPmp.drop(LduCnt).take(StaCnt)
  private val fromStDCache = fromDCache.drop(LduCnt).take(StaCnt)
  staUnitImps.zipWithIndex.map {
    case (impl: StoreAddrUnitImp, i) =>
      impl.io.fromCtrl.redirect <> fromCtrl.redirect
      impl.io.fromCtrl.csrCtrl  <> fromCtrl.csrCtrl
      if (impl.io.fromCtrl.trigger.isDefined) {
        impl.io.fromCtrl.trigger.get <> fromCtrl.trigger
      }

      val assignedIqIssue = 0 +:
                            staUnitImps.take(i).map(
                              _.params.issueParams.filter(_.isIq).length
                            )
      val assignedPrefetchIssue = 0 +:
                            staUnitImps.take(i).map(
                              _.params.issueParams.filter(_.isPrefetch).length
                            )
      val assignedMisalignBufIssue = 0 +:
                            staUnitImps.take(i).map(
                              _.params.issueParams.filter(_.isMisalignBuf).length
                            )
      val assignedVectorStoreIssue = 0 +:
                            staUnitImps.take(i).map(
                              _.params.issueParams.filter(_.isVectorStore).length
                            )
      var iqSelect  = 0
      var pfSelect  = 0
      var mabSelect = 0
      var vecSelect = 0

      impl.io.fromIssue.zip(impl.params.issueParams).map {
        case (sink, issueParam) =>
          true match {
            case _ if (issueParam.isIq) =>
              val iqIssue = io.fromBackend.issueSta.
                                          drop(assignedIqIssue.sum).
                                            drop(iqSelect).take(1).head
              sink.valid := iqIssue.valid
              sink.bits.fromMemExuInputBundle(iqIssue.bits, isStore = true)
              sink.bits.isIq := true.B
              iqIssue.ready := sink.ready
              iqSelect += 1

            case _ if (issueParam.isPrefetch) =>
              val pfIssue = io.fromPrefetch.store.
                                            drop(assignedPrefetchIssue.sum).
                                              drop(pfSelect).take(1).head
              sink.valid := pfIssue.valid
              sink.bits.fromStorePrefetchReqBundle(pfIssue.bits)
              pfIssue.ready := sink.ready
              pfSelect += 1

            case _ if (issueParam.isMisalignBuf) =>
              val mabIssue = io.fromStoreMisalignBuf.
                                            drop(assignedMisalignBufIssue.sum).
                                              drop(mabSelect).take(1).head
              sink.valid := mabIssue.valid
              sink.bits  := mabIssue.bits
              sink.bits.isMisalignBuf := true.B
              mabIssue.ready := sink.ready
              mabSelect += 1

            case _ if (issueParam.isVectorStore) =>
              val vecIssue = io.fromVectorStore.
                                          drop(assignedVectorStoreIssue.sum).
                                            drop(vecSelect).take(1).head
              sink.valid := vecIssue.valid
              sink.bits.fromVecPipeBundle(vecIssue.bits, isStore = true)
              vecIssue.ready := sink.ready
              vecSelect += 1

            case _ => require(false, "StoreAddrUnit dosen't support issue type: " + issueParam.issueType)
          }
      }
      toBackend.staIqFeedback(i).feedbackSlow <> impl.io.toBackend.iqFeedback
      toBackend.staIqFeedback(i).feedbackFast := DontCare
      toBackend.stIssue(i) <> impl.io.toBackend.issue

      // to tlb
      toStTlb(i).req <> impl.io.toTlb.req
      toStTlb(i).req_kill := impl.io.toTlb.req_kill

      // from tlb
      impl.io.fromTlb <> fromStTlb(i)
      impl.io.fromPmp <> fromStPmp(i)

      // to dcache
      toStDCache(i) <> impl.io.toDCache
      impl.io.fromDCache <> fromStDCache(i)

      // to misalignbuf
      toStoreMisalignBuf.enq(i) <> impl.io.toMisalignBuf.enq
      if (impl.params.hasMisalignExe) {
        toStoreMisalignBuf.wb <> impl.io.toMisalignBuf.wb.get
      }

      // to prefetch
      toPrefetch.store(i) <> impl.io.toPrefetch

      // to lsq
      io.toLsq.addrRelatedUpdate(i) <> impl.io.toLsq.addrRelatedUpdate
      io.toLsq.excpRelatedUpdate(i) <> impl.io.toLsq.excpRelatedUpdate
      io.toLsq.maskOut(i) := impl.io.toLsq.maskOut

      // to Ldu
      io.toLdu(i) <> impl.io.toLdu
  }
  // writeback
  val scalarWritebackPorts = staUnitImps.map(_.params.issueParams).flatten.filter(_.isIq).map(_.wbPort).distinct
  val scalarWriteback = staUnitImps.zip(scalarWritebackPorts).map {
    case (unit, portIdx) =>
      unit.io.toIssue(portIdx)
  }
  io.toBackend.writebackSta.zip(scalarWriteback).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toMemExuOutputBundle()
      source.ready := sink.ready
  }
  val vectorWritebackPorts = staUnitImps.map(_.params.issueParams).flatten.filter(_.isVector).map(_.wbPort).distinct
  val vectorWriteback = staUnitImps.zip(vectorWritebackPorts).map {
    case (unit, portIdx) =>
      unit.io.toIssue(portIdx)
  }
  io.toVectorStore.zip(vectorWriteback).map {
    case (sink, source) =>
      sink.valid := source.valid
      sink.bits  := source.bits.toVecPipelineFeedbackBundle(isVStore = true)
      source.ready := sink.ready
  }
}