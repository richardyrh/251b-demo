// See README.md for license details.

package gcd

import chisel3._
import chisel3.util.MixedVec
import circt.stage.ChiselStage

class MatrixReduction(rows: Int, cols: Int) extends Module {
  val dataT = UInt(32.W)

  val io = IO(new Bundle {
    val tensor = Input(Vec(rows, Vec(cols, dataT)))
    val rowSums = Output(Vec(rows, dataT)) // sum in the column dimension
    val colSums = Output(Vec(cols, dataT)) // sum in the row dimension
  })

  io.rowSums := io.tensor.map(col => col.reduce(_ + _))
  io.colSums := io.tensor.transpose.map(row => row.reduce(_ + _))
}

class NDArrayReduction(dims: Seq[Int]) extends Module {

  def createTensor[T <: UInt](dims: Seq[Int], gen: T): Vec[Data] = {
    dims match {
      case x :: Nil =>
        Vec(x, gen)
      case x :: y =>
        Vec(x, createTensor(y, gen))
    }
  }

  def cartesianProduct[A](xss: Seq[Seq[A]]): Seq[Vector[A]] =
    xss.foldLeft(Seq(Vector.empty[A])) { (acc, xs) =>
      for {
        a <- acc
        x <- xs
      } yield (x +: a)
    }.map(_.reverse)

  def indexNested(tensor: Any, path: Seq[Int]): UInt =
    path.foldLeft(tensor) {
      case (cur, i) =>
        cur match {
          case s: Vec[?] => s(i)
        }
    }.asInstanceOf[UInt]

  val dataT = UInt(32.W)

  val io = IO(new Bundle {
    val tensor = Input(createTensor(dims, dataT))
    val sums = Output(MixedVec(dims.map(Vec(_, dataT))))
  })

  val indices = cartesianProduct(dims.map(0 until _))

  io.sums.zipWithIndex.foreach { case (v, dim) =>
    v.zipWithIndex.foreach { case (sum, indexInDim) =>
      // filter where index at dimension `dim` is equal to `indexInDim`
      val relevantIndices = indices.filter(ii => ii(dim) == indexInDim)
      // for each index path, pick out the element
      val relevantElements = relevantIndices.map(ii => indexNested(io.tensor, ii))
      // sum them
      sum := relevantElements.reduce(_ + _)
    }
  }
}


object MatrixReduction extends App {
  println(ChiselStage.emitSystemVerilog(
    new MatrixReduction(8, 16),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  ))
}

object NDArrayReduction extends App {
  println(ChiselStage.emitSystemVerilog(
    new NDArrayReduction(Seq(8, 16, 32)),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  ))
}