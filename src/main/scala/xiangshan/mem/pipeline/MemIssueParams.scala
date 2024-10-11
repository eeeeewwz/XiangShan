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
import freechips.rocketchip.util.property
import xiangshan._
import xiangshan.backend.fu.NewCSR.MemType
import scala.language.implicitConversions

case class MemIssueParams(
  name:           String,
  issueType:      MemIssueType.Type,
  rfWen:          Boolean = false,
  wbPort:         Int     = -1,
  trigger:        Boolean = false,
  delayedError:   Boolean = false,
  exceptionOut:   Seq[Int] = Seq(),
) {
  def isNone:         Boolean = issueType == MemIssueType.None
  def isStoreData:    Boolean = issueType == MemIssueType.StoreData
  def isStoreAddr:    Boolean = issueType == MemIssueType.StoreAddr
  def isScalarLoad:   Boolean = issueType == MemIssueType.ScalarLoad
  def isVectorLoad:   Boolean = issueType == MemIssueType.VectorLoad
  def isVectorStore:  Boolean = issueType == MemIssueType.VectorStore
  def isAtomic:       Boolean = issueType == MemIssueType.Atomic
  def isUncache:      Boolean = issueType == MemIssueType.Uncache
  def isPrefetch:     Boolean = issueType == MemIssueType.Prefetch
  def isMisalignBuf:  Boolean = issueType == MemIssueType.MisalignBuf
  def isFastReplay:   Boolean = issueType == MemIssueType.FastReplay
  def isFastWakeup:   Boolean = issueType == MemIssueType.FastWakeup
  def isLoadReplay:   Boolean = issueType == MemIssueType.LoadReplay

  def isIq: Boolean = {
    isStoreData || isStoreAddr || isScalarLoad || isAtomic
  }

  def isStore: Boolean = {
    isStoreData || isStoreAddr || isVectorStore
  }

  def isLoad: Boolean = {
    isScalarLoad || isVectorLoad
  }

  def isVector: Boolean = isVectorStore || isVectorLoad

  def hasAGU: Boolean = {
    isStoreAddr || isScalarLoad
  }

  def hasTlbQuery: Boolean = {
    isMisalignBuf || isLoadReplay || isStoreAddr || isScalarLoad || isVectorLoad || isVectorStore || isFastWakeup
  }

  def hasDCacheQuery: Boolean = {
    hasTlbQuery || isPrefetch || isFastReplay
  }

  def triggerType: Boolean =
    if (isStoreAddr || isVectorStore)
      MemType.STORE
    else
      MemType.LOAD
}

object MemIssueType {
  sealed abstract class Type(val id: Int) {
    def U: UInt = id.U(width.W)
  }

  val all: Set[Type] = Set(
    None,
    StoreData,
    StoreAddr,
    ScalarLoad,
    VectorLoad,
    VectorStore,
    Atomic,
    Uncache,
    Prefetch,
    MisalignBuf,
    FastReplay,
    FastWakeup,
    LoadReplay
  )
  val width = log2Up(all.size)
  def chiselType() = UInt(width.W)

  case object None        extends Type(0)
  case object StoreData   extends Type(1)
  case object StoreAddr   extends Type(2)
  case object ScalarLoad  extends Type(3)
  case object VectorLoad  extends Type(4)
  case object VectorStore extends Type(5)
  case object Atomic      extends Type(6)
  case object Uncache     extends Type(7)
  case object Prefetch    extends Type(8)
  case object MisalignBuf extends Type(9)
  case object FastReplay  extends Type(10)
  case object FastWakeup  extends Type(11)
  case object LoadReplay  extends Type(12)

  def apply(): Type = None
}
