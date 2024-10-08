/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._
import xiangshan.mem.prefetch.PrefetchReqBundle
import math._

object Bundles {

  class LsPipelineBundle(val isVectorBundle: Boolean = false)(implicit p: Parameters) extends XSBundle
    with HasTlbConst
    with HasDCacheParameters
    with HasVLSUParameters
  {
    // common
    val uop                 = new DynInst
    val src                 = if (isVectorBundle) Vec(5, UInt(VLEN.W)) else Vec(3, UInt(XLEN.W))
    val vaddr               = UInt(VAddrBits.W)
    val paddr               = UInt(PAddrBits.W)
    val gpaddr              = UInt(XLEN.W)
    val fullva              = UInt(XLEN.W)
    val mask                = UInt((VLEN/8).W)
    val data                = UInt((VLEN+1).W)
    val miss                = Bool()
    val tlbMiss             = Bool()
    val ptwBack             = Bool()
    val af                  = Bool()
    val mmio                = Bool()
    val atomic              = Bool()
    val wlineflag           = Bool() // store write the whole cache line
    val vaNeedExt           = Bool()
    val isStore             = Bool()
    val isHyper             = Bool()
    val isVector            = Bool()
    val isForVSnonLeafPTE   = Bool()
    val is128bit            = Bool()
    val dataWenDup          = Vec(6, Bool())
    // replay
    val replayCarry         = new ReplayCarry(nWays)
    val lastBeat            = Bool()
    val mshrHandled         = Bool()
    val mshrId              = UInt(log2Up(cfg.nMissEntries).W)
    val tlbHandled          = Bool()
    val tlbId               = UInt(log2Up(loadfiltersize).W)
    val dataInvalidSqIdx    = new SqPtr
    val addrInvalidSqIdx    = new SqPtr
    val replacementUpdated  = Bool()
    val missDbUpdated       = Bool()
    val forwardTLDchannel   = Bool()
    val dcacheRequireReplay = Bool()
    val hasROBEntry         = Bool()
    val schedIdx            = UInt(log2Up(LoadQueueReplaySize).W)
    val causeVec            = ReplayCauseVec()
    // execute
    val isPrefetch          = Bool()
    val isHWPrefetch        = Bool()
    val isMisalignBuf       = Bool()
    val isLoadReplay        = Bool()
    val isFastWakeup        = Bool()
    val isFastReplay        = Bool()
    val isFirstIssue        = Bool()
    val tlbNoQuery          = Bool()
    val delayedError        = Bool()
    val feedbacked          = Bool()
    val ldCancel            = ValidUndirectioned(UInt(log2Ceil(LoadPipelineWidth).W))
    // vector
    val flowNum             = NumLsElem()
    val firstEle            = Bool()
    val lastElem            = Bool()
    val unitStrideFof       = Bool()
    val usSecondInv         = Bool()
    val elemIdx             = UInt(elemIdxBits.W)
    val alignedType         = UInt(alignTypeBits.W)
    val mbIdx               = UInt(max(vlmBindexBits, vsmBindexBits).W)
    val regOffset           = UInt(vOffsetBits.W)
    val elemIdxInsideVd     = UInt(elemIdxBits.W)
    val vecActive           = Bool() // 1: vector active element or scala mem operation, 0: vector not active element
    val vecBaseVaddr        = UInt(VAddrBits.W)
    val vecVaddrOffset      = UInt(VAddrBits.W)
    val vecTriggerMask      = UInt((VLEN/8).W)

    def isSWPrefetch: Bool  = isPrefetch && !isHWPrefetch

    def needReplay: Bool    = causeVec.asUInt.orR

    def fromMemExuInputBundle(input: MemExuInput, isStore: Boolean = false) = {
      this       := 0.U.asTypeOf(this)
      this.uop   := input.uop
      this.src   := input.src
      this.isStore := isStore.B
      this.isFirstIssue := input.isFirstIssue
      this.flowNum := input.flowNum.getOrElse(0.U.asTypeOf(this.flowNum))
    }

    def toMemExuOutputBundle(isVector: Boolean = false): MemExuOutput = {
      val res = Wire(new MemExuOutput(isVector = isVector))
      res.uop  := this.uop
      res.data := this.data
      res.debug.isMMIO    := this.mmio
      res.debug.vaddr     := this.vaddr
      res.debug.paddr     := this.paddr
      res.debug.isPerfCnt := false.B
      res
    }
    def fromVecPipeBundle(input: VecPipeBundle, isStore: Boolean = false): LsPipelineBundle = {
      val res = Wire(new LsPipelineBundle)
      res                 := DontCare
      res.uop             := input.uop
      res.vaddr           := input.vaddr
      res.vecBaseVaddr    := input.basevaddr
      res.mask            := input.mask
      res.isVector        := input.isvec
      res.isStore         := isStore.B
      res.unitStrideFof   := input.uop_unit_stride_fof
      res.regOffset       := input.reg_offset
      res.alignedType     := input.alignedType
      res.vecActive       := input.vecActive
      res.firstEle        := input.is_first_ele
      res.isFirstIssue    := input.isFirstIssue
      res.usSecondInv     := input.usSecondInv
      res.mbIdx           := input.mBIndex
      res.elemIdx         := input.elemIdx
      res.elemIdxInsideVd := input.elemIdxInsideVd
      res
    }

    def toVecPipelineFeedbackBundle(isVStore: Boolean = false): VecPipelineFeedbackIO = {
      val res = Wire(new VecPipelineFeedbackIO(isVStore = isVStore))
      res.mBIndex         := this.mbIdx
      res.hit             := this.feedbacked && this.isVector
      res.isvec           := this.isVector
      res.flushState      := DontCare
      res.sourceType      := Mux(isVStore.B, RSFeedbackType.tlbMiss, RSFeedbackType.lrqFull)
      res.trigger         := this.uop.trigger
      res.mmio            := this.mmio
      res.exceptionVec    := this.uop.exceptionVec
      res.usSecondInv     := this.usSecondInv
      res.vecFeedback     := this.feedbacked && this.isVector
      res.elemIdx         := this.elemIdx
      res.alignedType     := this.alignedType
      res.mask            := this.mask
      res.vaddr           := this.vaddr
      res.vaNeedExt       := this.vaNeedExt
      res.gpaddr          := this.gpaddr
      res.isForVSnonLeafPTE := this.isForVSnonLeafPTE
      res.vstart          := this.uop.vpu.vstart
      res.vecTriggerMask  := this.vecTriggerMask
      if (!isVStore) {
        res.reg_offset.get      := this.regOffset
        res.elemIdxInsideVd.get := this.elemIdxInsideVd
      }
      // res.vecdata
      res
    }

    def toVecMemOutputBundle(): VecMemExuOutput = {
      val res = Wire(new VecMemExuOutput(isVector = true))
      res.output        := this.toMemExuOutputBundle(isVector = true)
      res.vecFeedback   := this.feedbacked && this.isVector
      res.mmio          := this.mmio
      res.usSecondInv   := this.usSecondInv
      res.elemIdx       := this.elemIdx
      res.alignedType   := this.alignedType
      res.mbIndex       := this.mbIdx
      res.mask          := this.mask
      res.vaddr         := this.vaddr
      res.gpaddr        := this.gpaddr
      res.vaNeedExt     := this.vaNeedExt
      res.isForVSnonLeafPTE := this.isForVSnonLeafPTE
      res
    }
  }

  class LsPrefetchTrainBundle(implicit p: Parameters) extends LsPipelineBundle {
    val metaPrefetch = UInt(L1PfSourceBits.W)
    val metaAccess   = Bool()

    def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
      val inputReg = latch match {
        case true   => RegEnable(input, enable)
        case false  => input
      }
      uop               := inputReg.uop
      vaddr             := inputReg.vaddr
      fullva            := inputReg.fullva
      paddr             := inputReg.paddr
      gpaddr            := inputReg.gpaddr
      isForVSnonLeafPTE := inputReg.isForVSnonLeafPTE
      vaNeedExt         := inputReg.vaNeedExt
      isStore           := inputReg.isStore
      isHyper           := inputReg.isHyper
      mask              := inputReg.mask
      data              := inputReg.data
      wlineflag         := inputReg.wlineflag
      miss              := inputReg.miss
      tlbMiss           := inputReg.tlbMiss
      ptwBack           := inputReg.ptwBack
      af                := inputReg.af
      mmio              := inputReg.mmio
      isPrefetch        := inputReg.isPrefetch
      isHWPrefetch      := inputReg.isHWPrefetch
      isMisalignBuf     := inputReg.isMisalignBuf
      isFirstIssue      := inputReg.isFirstIssue
      hasROBEntry       := inputReg.hasROBEntry
      dcacheRequireReplay := inputReg.dcacheRequireReplay
      schedIdx          := inputReg.schedIdx
      tlbNoQuery        := inputReg.tlbNoQuery
      isVector          := inputReg.isVector
      lastElem          := inputReg.lastElem
      is128bit          := inputReg.is128bit
      vecActive         := inputReg.vecActive
      firstEle          := inputReg.firstEle
      unitStrideFof     := inputReg.unitStrideFof
      usSecondInv       := inputReg.usSecondInv
      regOffset         := inputReg.regOffset
      elemIdx           := inputReg.elemIdx
      alignedType       := inputReg.alignedType
      mbIdx             := inputReg.mbIdx
      elemIdxInsideVd   := inputReg.elemIdxInsideVd
      vecBaseVaddr      := inputReg.vecBaseVaddr
      vecVaddrOffset    := inputReg.vecVaddrOffset
      vecTriggerMask    := inputReg.vecTriggerMask
      src               := DontCare
      metaPrefetch      := DontCare
      metaAccess        := DontCare
      forwardTLDchannel := DontCare
      mshrHandled       := DontCare
      mshrId            := DontCare
      replayCarry       := DontCare
      atomic            := DontCare
      isLoadReplay      := DontCare
      isFastWakeup      := DontCare
      isFastReplay      := DontCare
      replacementUpdated := DontCare
      missDbUpdated     := DontCare
      delayedError      := DontCare
      feedbacked        := DontCare
      ldCancel          := DontCare
      dataWenDup        := DontCare
      causeVec          := DontCare
      lastBeat          := DontCare
      tlbHandled        := DontCare
      tlbId             := DontCare
      dataInvalidSqIdx  := DontCare
      addrInvalidSqIdx  := DontCare
      flowNum           := DontCare
    }

    def asPrefetchReqBundle(): PrefetchReqBundle = {
      val res = Wire(new PrefetchReqBundle)
      res.vaddr       := this.vaddr
      res.paddr       := this.paddr
      res.pc          := this.uop.pc
      res.miss        := this.miss
      res.pfHitStream := isFromStream(this.metaPrefetch)

      res
    }
  }

  class LsPrefetchTrainIO(implicit p: Parameters) extends XSBundle {
    val req = ValidIO(new LsPrefetchTrainBundle)
    val s1PrefetchSpec = Output(Bool())
    val s2PrefetchSpec = Output(Bool())
  }

  class LoadForwardReqBundle(implicit p: Parameters) extends XSBundle {
    val uop           = new DynInst
    val vaddr         = UInt(VAddrBits.W)
    val paddr         = UInt(PAddrBits.W)
    val mask          = UInt((VLEN/8).W)
    val pc            = UInt(VAddrBits.W) //for debug
    val sqIdx         = new SqPtr
    val sqIdxMask     = UInt(StoreQueueSize.W)
  }

  class LoadForwardRespBundle(implicit p: Parameters) extends XSBundle {
    val forwardMaskFast   = Vec((VLEN/8), Bool())
    val forwardMask       = Vec((VLEN/8), Bool())
    val forwardData       = Vec((VLEN/8), UInt(8.W))
    // dataInvalid suggests store to load forward found forward should happen,
    // but data is not available for now. If dataInvalid, load inst should
    // be replayed from RS. Feedback type should be RSFeedbackType.dataInvalid
    val dataInvalid       = Bool()
    val dataInvalidFast   = Bool()
    val dataInvalidSqIdx  = new SqPtr
    // matchInvalid suggests in store to load forward logic, paddr cam result does
    // to equal to vaddr cam result. If matchInvalid, a microarchitectural exception
    // should be raised to flush SQ and committed sbuffer.
    val matchInvalid      = Bool()
    // addrInvalid suggests store to load forward found forward should happen,
    // but address (SSID) is not available for now. If addrInvalid, load inst should
    // be replayed from RS. Feedback type should be RSFeedbackType.addrInvalid
    val addrInvalid       = Bool()
    val addrInvalidSqIdx  = new SqPtr
  }

  class LoadForwardIO(implicit p: Parameters) extends XSBundle {
    val req  = Valid(new LoadForwardReqBundle)
    val resp = Input(new LoadForwardRespBundle)
  }

  // Query load queue for ld-ld violation
  //
  // Req should be send in load_s1
  // Resp will be generated 1 cycle later
  //
  // Note that query req may be !ready, as dcache is releasing a block
  // If it happens, a replay from rs is needed.
  class LoadNukeQueryReqBundle(implicit p: Parameters) extends XSBundle { // provide lqIdx
    val uop         = new DynInst
    val mask        = UInt((VLEN/8).W)
    val paddr       = UInt(PAddrBits.W)
    val dataValid   = Bool()
  }

  class LoadNukeQueryRespBundle(implicit p: Parameters) extends XSBundle {
    // replayFromFetch: ld-ld violation check success, replay from fetch.
    val replayFromFetch = Bool()
  }

  class LoadNukeQueryIO(implicit p: Parameters) extends XSBundle {
    val req    = Decoupled(new LoadNukeQueryReqBundle)
    val resp   = Flipped(Valid(new LoadNukeQueryRespBundle))
    val revoke = Output(Bool())
  }

  class StoreNukeQueryBundle(implicit p: Parameters) extends XSBundle {
    val robIdx    = new RobPtr
    val paddr     = UInt(PAddrBits.W)
    val mask      = UInt((VLEN/8).W)
    val matchLine = Bool()
  }

  class StoreMaBufToSqCtrlControlBundle(implicit p: Parameters) extends XSBundle {
    val writeSb       = Bool()
    val wdata         = UInt(VLEN.W)
    val wmask         = UInt((VLEN / 8).W)
    val paddr         = UInt(PAddrBits.W)
    val vaddr         = UInt(VAddrBits.W)
    val last          = Bool()
    val hasException  = Bool()
    // remove this entry in sq
    val removeSq      = Bool()
  }

  class StoreMaBufToSqCtrlStoreInfoBundle(implicit p: Parameters) extends XSBundle {
    val data            = UInt(VLEN.W)
    // is the data of the unaligned store ready at sq?
    val dataReady       = Bool()
    // complete a data transfer from sq to sb
    val completeSbTrans = Bool()
  }

  class StoreMaBufToSqCtrlIO(implicit p: Parameters) extends XSBundle {
    // from storeMisalignBuffer to storeQueue, control it's sbuffer write
    val control   = Output(new StoreMaBufToSqCtrlControlBundle)
    // from storeQueue to storeMisalignBuffer, provide detail info of this store
    val storeInfo = Input(new StoreMaBufToSqCtrlStoreInfoBundle)
  }

  // Store byte valid mask write bundle
  //
  // Store byte valid mask write to SQ takes 2 cycles
  class StoreMaskBundle(implicit p: Parameters) extends XSBundle {
    val sqIdx = new SqPtr
    val mask = UInt((VLEN/8).W)
  }


  class LoadDataFromDcacheBundle(implicit p: Parameters) extends DCacheBundle {
    // old dcache: optimize data sram read fanout
    // val bankedDcacheData = Vec(DCacheBanks, UInt(64.W))
    // val bank_oh = UInt(DCacheBanks.W)
    val uop                 = new DynInst // for data selection, only fwen and fuOpType are used
    val respDcacheData      = UInt(VLEN.W)
    val forwardMask         = Vec(VLEN/8, Bool())
    val forwardData         = Vec(VLEN/8, UInt(8.W))
    val addrOffset          = UInt(4.W) // for data selection
    // forward tilelink D channel
    val forwardDchannel     = Bool()
    val forwardDataDchannel = Vec(VLEN/8, UInt(8.W))
    // forward mshr data
    val forwardMSHR         = Bool()
    val forwardDataMSHR     = Vec(VLEN/8, UInt(8.W))
    val forwardResultValid  = Bool()

    def mergeTLData(): UInt = {
      // merge TL D or MSHR data at load s2
      val dcacheData = respDcacheData
      val useDchannel = forwardDchannel && forwardResultValid
      val useMSHR = forwardMSHR && forwardResultValid
      Mux(
        useDchannel || useMSHR,
        Mux(
          useDchannel,
          forwardDataDchannel.asUInt,
          forwardDataMSHR.asUInt
        ),
        dcacheData
      )
    }

    def mergeLsqFwdData(dcacheData: UInt): UInt = {
      // merge dcache and lsq forward data at load s3
      val rdataVec = VecInit((0 until VLEN / 8).map(j =>
        Mux(forwardMask(j), forwardData(j), dcacheData(8*(j+1)-1, 8*j))
      ))
      rdataVec.asUInt
    }
  }

  // Load writeback data from load queue (refill)
  class LoadDataFromLQBundle(implicit p: Parameters) extends XSBundle {
    val lqData = UInt(64.W) // load queue has merged data
    val uop = new DynInst // for data selection, only fwen and fuOpType are used
    val addrOffset = UInt(3.W) // for data selection

    def mergedData(): UInt = {
      lqData
    }
  }

  // Bundle for load / store wait waking up
  class MemWaitUpdateReq(implicit p: Parameters) extends XSBundle {
    val robIdx = Vec(backendParams.StaExuCnt, ValidIO(new RobPtr))
    val sqIdx = Vec(backendParams.StdCnt, ValidIO(new SqPtr))
  }

}