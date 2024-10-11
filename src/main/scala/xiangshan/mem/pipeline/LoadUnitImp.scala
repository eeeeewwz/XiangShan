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

class LoadUnitIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO {

}

class LoadUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
{
  io.suggestName("none")
  override lazy val io = IO(new LoadUnitIO).suggestName("io")

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  io.fromIssue.zip(params.issueParams).zipWithIndex.map {
    case ((fromIssue, issueParam), i) =>
      val loadReplayStall =
        if (issueParam.isLoadReplay)
          io.fromIssue.zip(params.issueParams).filter(_._2.isIq).map {
            case (iqIssue, issueParam) =>
              iqIssue.valid && isAfter(fromIssue.bits.uop.robIdx, iqIssue.bits.uop.robIdx)
          }.reduce (_|_)
        else
          false.B

      val highConfidencePrefetch =
        if (issueParam.isIq || issueParam.isVector || issueParam.isFastWakeup)
          io.fromIssue.zip(params.issueParams).filter(_._2.isPrefetch).map {
            case (pfIssue, issueParam) =>
              pfIssue.valid && pfIssue.bits.confidence > 0.U
          }.reduce(_|_)
        else
          false.B

      val loadReplayDataRelated =
        if (issueParam.isUncache || issueParam.isFastReplay)
          io.fromIssue.zip(params.issueParams).filter(_._2.isLoadReplay).map {
            case (replayIssue, issueParams) =>
              replayInst.valid && !replayIssue.bits.forwardTLDchannel
          }
        else
          false.B

      val prioritySelect = (if (i == 0) true.B else !io.fromIssue.slice(0, i).map(_.valid).reduce(_|_))

      val selected = !loadReplayStall && !highConfidencePrefetch && !loadReplayDataRelated && prioritySelect

      when (fromIssue.valid && selected) {
        if (issueParam.wbPort > 0)
          s0WBPort(issueParam.wbPort) := true.B
        s0ArbOut.valid       := fromIssue.valid
        s0ArbOut.bits        := fromIssue.bits
        s0ArbOut.bits.vecActive := !fromIssue.bits.isVector || fromIssue.bits.vecActive
        fromIssue.ready   := s0ArbOut.ready && (if (issueParam.hasDCacheQuery) io.toDCache.req.ready else true.B)
      }
  }
}