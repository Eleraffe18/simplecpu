package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

object Sel {
    // A:
    val rs1 = 0.U
    val pc  = 1.U
    // B:
    val rs2 = 0.U
    val imm = 1.U
    // O:
    val alu = 0.U
    val mem = 1.U
}

object RV64I {
    //                 |      imm    |
    //                 |funct7 | rs2 | rs1 |f3 | rd  |opcode |
    val addi  = BitPat("b???????_?????_?????_000_?????_0010011");
    val add   = BitPat("b0000000_?????_?????_000_?????_0110011");
    val sd    = BitPat("b???????_?????_?????_011_?????_0100011");
    val auipc = BitPat("b???????_?????_?????_???_?????_0010111");
    val lui   = BitPat("b???????_?????_?????_???_?????_0110111");
    val addiw = BitPat("b???????_?????_?????_000_?????_0011011");
    val addw  = BitPat("b0000000_?????_?????_000_?????_0111011");
}

object InstType {
    val R = 0.U
    val I = 1.U
    val S = 2.U
    val B = 3.U
    val U = 4.U
    val J = 5.U
}

class DecodeSignal extends Bundle {
    val instType = UInt(3.W)
    val asel     = UInt(1.W)
    val bsel     = UInt(1.W)
    val osel     = UInt(2.W)
    val memEn    = Bool()
    val memWe    = Bool()
    val rdWe     = Bool()
    val brCond   = UInt(5.W)
    val aluop    = UInt(4.W)
    val isJal    = Bool()
    val cut32    = Bool()
    val err      = Bool()
}

object DecodeSignal {
    def apply(args: List[Data]) = {
        val res = Wire(new DecodeSignal)
        res.instType := args(0)
        res.asel     := args(1)
        res.bsel     := args(2)
        res.osel     := args(3)
        res.memEn    := args(4)
        res.memWe    := args(5)
        res.rdWe     := args(6)
        res.brCond   := args(7)
        res.aluop    := args(8)
        res.isJal    := args(9)
        res.cut32    := args(10)
        res.err      := args(11)
        res
    }
}

class DecodeIO extends Bundle {
    val inst = Input(UInt(32.W))
    val imm  = Output(UInt(DW.W))
    val sig  = Output(new DecodeSignal)
}

class Decode extends Module {

    val io = IO(new DecodeIO)

    val signals = Wire(new DecodeSignal)
    val inst    = io.inst

    import InstType._
    import Sel._
    import AluOpcode._

    val F = false.B
    val T = true.B

    signals := DecodeSignal(
        ListLookup(
            inst,
            List(I, rs1, imm, alu, F, F, F, "b00000".U, add, F, F, T),
            Array(
                //                                                             isJal
                //                  T  sa   sb   so men mwe rd   bcond    alu   | 32 err
                RV64I.addi  -> List(I, rs1, imm, alu, F, F, T, "b00000".U, add, F, F, F),
                RV64I.add   -> List(R, rs1, rs2, alu, F, F, T, "b00000".U, add, F, F, F),
                RV64I.sd    -> List(S, rs1, imm, mem, T, T, F, "b00000".U, add, F, F, F),
                RV64I.auipc -> List(U, pc, imm, alu, F, F, T, "b00000".U, add, F, F, F),
                RV64I.lui   -> List(U, rs1, imm, alu, F, F, T, "b00000".U, b, F, F, F),
                RV64I.addiw -> List(I, rs1, imm, alu, F, F, T, "b00000".U, add, F, T, F),
                RV64I.addw  -> List(R, rs1, rs2, alu, F, F, T, "b00000".U, add, F, T, F)
            )
        )
    )

    io.imm := 0.U
    switch(signals.instType) {
        is(I) { io.imm := Cat(Fill(52, inst(31)), inst(31, 20)) }
        is(S) { io.imm := Cat(Fill(52, inst(31)), inst(31, 25), inst(11, 7)) }
        is(B) { io.imm := Cat(Fill(52, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)) }
        is(U) { io.imm := Cat(Fill(32, inst(31)), inst(31, 12), 0.U(12.W)) }
        is(J) { io.imm := Cat(Fill(44, inst(31)), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)) }
    }

    io.sig := signals

}
