package simplecpu

import circt.stage.ChiselStage

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

object Main {
    def main(args: Array[String]): Unit = {
        ChiselStage.emitSystemVerilogFile(new Cpu, args)
    }
}