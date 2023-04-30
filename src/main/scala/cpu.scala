package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

class CpuMemIO extends Bundle {
    val addr  = Output(UInt(DW.W))
    val rdata = Input(UInt(DW.W))
    val wdata = Output(UInt(DW.W))
    val en    = Output(Bool())
    val we    = Output(Bool())
}

class CpuIO extends Bundle {
    val pc   = Output(UInt(DW.W))
    val inst = Output(UInt(32.W))
    val imem = new CpuMemIO
    val dmem = new CpuMemIO
    val gpr  = Vec(32, Output(UInt(DW.W)))
    val err  = Output(Bool())
}

class Cpu extends Module {

    val io = IO(new CpuIO)

    val pc   = RegInit("h80000000".U(DW.W))
    val ipc  = RegNext(pc)
    val inst = RegNext(Mux(pc(2), io.imem.rdata(63, 32), io.imem.rdata(31, 0)), "h00000013".U(32.W))

    val alu     = Module(new Alu)
    val decode  = Module(new Decode)
    val regfile = Module(new Regfile)

    val aluRes = Mux(decode.io.sig.cut32, Cat(Fill(32, alu.io.out(31)), alu.io.out(31, 0)), alu.io.out)

    io.pc := pc
    
    io.err := decode.io.sig.err

    io.imem.addr  := pc & (~(7.U(64.W)))
    io.imem.wdata := DontCare
    io.imem.en    := true.B
    io.imem.we    := false.B
    io.inst := inst

    decode.io.inst := inst

    regfile.io.rs1.addr := inst(19, 15)
    regfile.io.rs2.addr := inst(24, 20)
    regfile.io.rd.addr  := inst(11, 7)
    regfile.io.rd.data  := Mux(decode.io.sig.osel === Sel.alu, aluRes, io.dmem.rdata)
    regfile.io.rd.we    := decode.io.sig.rdWe

    alu.io.ua     := Mux(decode.io.sig.asel === Sel.rs1, regfile.io.rs1.data, ipc)
    alu.io.ub     := Mux(decode.io.sig.bsel === Sel.rs2, regfile.io.rs2.data, decode.io.imm)
    alu.io.opcode := decode.io.sig.aluop

    io.dmem.addr  := aluRes
    io.dmem.wdata := regfile.io.rs2.data
    io.dmem.en    := decode.io.sig.memEn
    io.dmem.we    := decode.io.sig.memWe

    io.gpr := regfile.io.gpr

    // update PC
    pc := Mux(
        (decode.io.sig.brCond & alu.io.cmp).orR,
        Mux(decode.io.sig.isJal, pc, regfile.io.rs1.data) + decode.io.imm,
        pc + 4.U
    )

}
