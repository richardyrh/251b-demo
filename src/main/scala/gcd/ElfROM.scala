// See README.md for license details.

package gcd

import chisel3._
import chisel3.util.{Cat, log2Ceil}

import java.nio.charset.StandardCharsets
import circt.stage.ChiselStage

import java.nio.file.{Files, Path}
import scala.sys.process.stringSeqToProcess

class AsciiStrings extends Module {
  class AsciiStringInHex(maxChars: Int = 128) extends Bundle {
    private def maxBits: Int = maxChars * 8
    val value = UInt((maxBits * 8).W) // little endian
    val length = UInt(log2Ceil(maxChars + 1).W)
  }

  def toHex(s: String, maxChars: Int = 128): AsciiStringInHex = {
    val raw = s.getBytes(StandardCharsets.US_ASCII)
    require(raw.length <= maxChars)

    // pad 0s in higher indices
    val padded = new Array[Byte](maxChars)
    System.arraycopy(raw, 0, padded, 0, raw.length)

    // concatenate bytes in little endian
    val bundle = Wire(new AsciiStringInHex(maxChars))
    bundle.value := Cat(padded.reverse.map(_.U(8.W)))
    bundle.length := raw.length.U

    bundle
  }

  val io = IO(new Bundle {
    val aaa = Output(new AsciiStringInHex(4))
    val hello = Output(new AsciiStringInHex(8))
    val deadBeef = Output(new AsciiStringInHex(16))
  })

  io.aaa := toHex("aaa")
  io.hello := toHex("hello")
  io.deadBeef := toHex("deaf beef")
}

class ElfROM(path: String) extends Module {

  def readTextSectionBytes(elfPath: Path): Array[Byte] = {
    // courtesy of chatgpt
    require(Files.isRegularFile(elfPath), s"not a file: $elfPath")

    val tmp = Files.createTempFile("elf-text-", ".bin")
    try {
      val cmd = Seq(
        "riscv32-unknown-elf-objcopy",
        "--only-section=.text",
        "--output-target=binary",
        elfPath.toString,
        tmp.toString
      )

      val exit = cmd.!
      if (exit != 0) throw new RuntimeException(s"objcopy failed (exit=$exit): ${cmd.mkString(" ")}")

      Files.readAllBytes(tmp) // Array[Byte]
    } finally {
      Files.deleteIfExists(tmp)
    }
  }

  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val data = Output(UInt(32.W))
  })

  val bytes = readTextSectionBytes(Path.of(path)).map(_.S(8.W).asUInt)
  val words = bytes.grouped(4).map(x => Cat(x.reverse)).toSeq

  val rom = VecInit(words)

  io.data := rom(io.addr)
}

object AsciiStrings extends App {
  val verilog = ChiselStage.emitSystemVerilog(
    new AsciiStrings(),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
  println(verilog)
}

object ElfROM extends App {
  println(ChiselStage.emitSystemVerilog(
    new ElfROM("hello.elf"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  ))
}
