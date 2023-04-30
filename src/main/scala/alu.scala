package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

object AluOpcode {
    val add  = "b0000".U
    val sub  = "b1000".U
    val sll  = "b0001".U
    val srl  = "b0101".U
    val sra  = "b1101".U
    val slt  = "b0010".U
    val sltu = "b0011".U
    val xor  = "b0100".U
    val or   = "b0110".U
    val and  = "b0111".U
    val b    = "b1110".U // for lui
    val add4 = "b1111".U // for jal and jalr
}

class AluIO extends Bundle {
    val opcode = Input(UInt(4.W))
    val ua     = Input(UInt(DW.W))
    val ub     = Input(UInt(DW.W))
    val out    = Output(UInt(DW.W))
    val cmp     = Output(UInt(5.W))
}

class Alu extends Module {

    val io = IO(new AluIO)

    val ua = io.ua
    val ub = io.ub
    val sa = io.ua.asSInt
    val sb = io.ub.asSInt

    val shamt32 = ub(4, 0)
    val shamt64 = ub(5, 0)
    val lt      = ua < ub
    val ltu     = sa < sb
    val eq      = ua === ub

    io.cmp := Cat(lt, ltu, eq, (!eq) && (!lt), (!eq) && (!ltu))

    io.out := 0.U

    switch(io.opcode) {
        is(AluOpcode.add) {
            io.out := ua + ub
        }
        is(AluOpcode.add4) {
            io.out := ua + 4.U
        } 
        is(AluOpcode.b) {
            io.out := ub
        } 
    }

}
