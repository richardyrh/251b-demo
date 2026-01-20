// See README.md for license details.

package gcd

import chisel3._
import chisel3.util.Log2

import scala.language.implicitConversions
import DPTypes._
import circt.stage.ChiselStage

abstract class CanDP[T](val v: T) {
  def mul(other: T): T
  def add(other: T): T
  implicit def box: T => CanDP[T]
}

object DPTypes {

  implicit class UIntDP(val x: UInt) extends CanDP[UInt](x) {
    override def mul(other: UInt): UInt = x * other
    override def add(other: UInt): UInt = x + other
    override implicit def box: UInt => CanDP[UInt] = UIntDP
  }

  // log dot product

  implicit class Log(val v: UInt) // auto convert UInt to Log (but not the other way)
  implicit class LogDP(val x: Log) extends CanDP[Log](x) {
    override def mul(other: Log): Log = x.v + other.v
    override def add(other: Log): Log = {
      // courtesy of chatgpt
      val m = x.v max other.v
      m + Log2((1.U << (x.v - m)).asUInt + (1.U << (other.v - m)).asUInt)
    }
    override implicit def box: Log => CanDP[Log] = LogDP
  }

  // symbolic dot product (that falls back onto UInt arithmetic)

  sealed trait TreeNode
  case class Add(l: TreeNode, r: TreeNode) extends TreeNode
  case class Mul(l: TreeNode, r: TreeNode) extends TreeNode
  case class Const(v: UInt) extends TreeNode

  implicit def nodeToUInt(node: TreeNode): UInt = {
    node match {
      case Add(l, r) => nodeToUInt(l) + nodeToUInt(r)
      case Mul(l, r) => nodeToUInt(l) * nodeToUInt(r)
      case Const(v) => v
    }
  }

  implicit class TreeNodeDP(val x: TreeNode) extends CanDP[TreeNode](x) {
    override def mul(other: TreeNode): TreeNode = Mul(x, other)
    override def add(other: TreeNode): TreeNode = Add(x, other)
    override implicit def box = TreeNodeDP
  }
}


class GenericDotProductUnit[T <: Data, U <: CanDP[T]]
    (dataT: T, n: Int)
    (implicit val cast: T => U) extends Module {

  val io = IO(new Bundle {
    val a = Input(Vec(n, dataT))
    val b = Input(Vec(n, dataT))
    val output = Output(dataT)
  })

  val products = (io.a zip io.b).map { case (x: T, y: T) =>
    x mul y
  }
  val s = products.reduce(_ add _)
  io.output := s
}

object GenericDotProductUnit extends App {
  val verilog = ChiselStage.emitSystemVerilog(
    new GenericDotProductUnit(UInt(32.W), 8),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
  println(verilog)
}
