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
import xiangshan.mem.Bundles.LsPipelineBundle

sealed trait MemUnitType

case class StoreDataUnit() extends MemUnitType
case class StoreAddrUnit() extends MemUnitType

case class MemUnitParams(
  name:         String        = "MemUnit",
  unitType:     MemUnitType,
  dataBits:     Int           = 128, // ignore it, don't change it
  issueParams:  Seq[MemIssueParams],
  hasPrefetch:  Boolean       = false,
) {

  def isStoreDataUnit: Boolean = unitType == StoreDataUnit()

  def isStoreAddrUnit: Boolean = unitType == StoreAddrUnit()

  def exceptionOut: Seq[Int] = issueParams.map(_.exceptionOut).reduce(_++_).distinct

  def numWBPorts: Int = issueParams.filter(_.wbPort >= 0).map(_.wbPort).distinct.length

  def hasTrigger: Boolean = issueParams.filter(_.trigger == true).length > 0

  def hasMisalignExe: Boolean = issueParams.filter(_.isMisalignBuf == true).length > 0

  def genLsPipelineSourceDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    MixedVec((0 until numWBPorts).map {
      case i =>
        val sameWBPortParams = issueParams.filter(_.wbPort == i)
        require(sameWBPortParams.map(_.isVector).distinct.length == 1, "The same writeback port must have same feature!")
        DecoupledIO(new LsPipelineBundle(isVectorBundle = sameWBPortParams.head.isVector))
    })
  }

  def genLsPipelineSinkDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    MixedVec(issueParams.map(x => Flipped(DecoupledIO(new LsPipelineBundle(isVectorBundle = x.isVector)))))
  }

  def unitTypeString: String = unitType match {
    case StoreDataUnit() => "Store Data"
    case StoreAddrUnit() => "Store Addr"
    case _ => "unknown"
  }
}