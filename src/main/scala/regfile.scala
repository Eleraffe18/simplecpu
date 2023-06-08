package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

class RegfileRsIO extends Bundle {
    val addr = Input(UInt(5.W))
    val data = Output(UInt(DW.W))
}

class RegfileRdIO extends Bundle {
    val addr = Input(UInt(5.W))
    val data = Input(UInt(DW.W))
    val we   = Input(Bool())
}

class RegfileIO extends Bundle {
    val rs1 = new RegfileRsIO
    val rs2 = new RegfileRsIO
    val rd  = new RegfileRdIO
    val gpr = Vec(32, Output(UInt(DW.W)))
}

class Regfile extends Module {

    val io = IO(new RegfileIO)

    val regs = Reg(Vec(32, UInt(DW.W)))

    List(io.rs1, io.rs2).foreach(x => {
        when(x.addr === 0.U) {
            x.data := 0.U
        }.otherwise {
            x.data := regs(x.addr)
        }
    })

    when((io.rd.we) && (io.rd.addr =/= 0.U)) {
        regs(io.rd.addr) := io.rd.data
    }

    io.gpr(0) := regs(0)
    for (i <- 1 until 32) {
        io.gpr(i) := regs(i)
    }

}
