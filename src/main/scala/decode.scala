package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

sealed trait DecodeInfo

sealed abstract class InstTypeInfo extends DecodeInfo {
    def instType: Int
}

case object RType extends InstTypeInfo {
    val instType = 0
}

case object IType extends InstTypeInfo {
    val instType = 1
}

case object SType extends InstTypeInfo {
    val instType = 2
}

case object BType extends InstTypeInfo {
    val instType = 3
}

case object UType extends InstTypeInfo {
    val instType = 4
}

case object JType extends InstTypeInfo {
    val instType = 5
}

sealed abstract class AInfo extends DecodeInfo {
    def sel: Int
}

case object AFromRs1 extends AInfo {
    val sel = 0
}

case object AFromPC extends AInfo {
    val sel = 1
}

sealed abstract class BInfo extends DecodeInfo {
    def sel: Int
}

case object BFromRs2 extends BInfo {
    val sel = 0
}

case object BFromImm extends BInfo {
    val sel = 1
}

sealed abstract class RdInfo extends DecodeInfo {
    def we:  Boolean
    def sel: Int
}

case object NoRd extends RdInfo {
    val we  = false
    val sel = 0
}

case object RdFromAlu extends RdInfo {
    val we  = true
    val sel = 0
}

case object RdFromMem extends RdInfo {
    val we  = true
    val sel = 1
}

sealed abstract class MemInfo extends DecodeInfo {
    def en: Boolean
    def we: Boolean
}

case object NoMem extends MemInfo {
    val en = false
    val we = false
}

case object MemRead extends MemInfo {
    val en = true
    val we = false
}

case object MemWrite extends MemInfo {
    val en = true
    val we = true
}

case class BranchInfo[T <: Data](cond: T) extends DecodeInfo

case object Cut32 extends DecodeInfo

case class AluOpInfo[T <: Data](op: T) extends DecodeInfo

case object InstIsJal extends DecodeInfo

case object DecodeErr extends DecodeInfo

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
    def apply(param: Array[DecodeInfo]) = {
        val sig = Wire(new DecodeSignal)
        sig := 0.U.asTypeOf(new DecodeSignal)
        param.foreach((nparam) => {
            nparam match {
                case n: InstTypeInfo => { sig.instType := n.instType.U }
                case n: AInfo        => { sig.asel := n.sel.U }
                case n: BInfo        => { sig.bsel := n.sel.U }
                case n: RdInfo       => { sig.rdWe := n.we.B; sig.osel := n.sel.U }
                case n: MemInfo      => { sig.memEn := n.en.B; sig.memWe := n.we.B }
                case BranchInfo(cond) => { sig.brCond := cond }
                case AluOpInfo(op)    => { sig.aluop := op }
                case InstIsJal        => { sig.isJal := true.B }
                case Cut32            => { sig.cut32 := true.B }
                case DecodeErr        => { sig.err := true.B }
            }
        })
        sig
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

    import AluOpcode._

    object insts extends simplecpu.inst.RV64I

    val F = false.B
    val T = true.B

    signals := MuxCase(
        DecodeSignal(Array(DecodeErr)),
        Seq(
            (inst === insts.lui)   -> DecodeSignal(Array(UType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(b))),
            (inst === insts.auipc) -> DecodeSignal(Array(UType, AFromPC, BFromImm, RdFromAlu, NoMem, AluOpInfo(add))),
            // --
            (inst === insts.jal) -> DecodeSignal(
                Array(JType, AFromPC, BFromImm, RdFromAlu, NoMem, AluOpInfo(add4), InstIsJal, BranchInfo("b11111".U))
            ),
            (inst === insts.jalr) -> DecodeSignal(
                Array(IType, AFromPC, BFromImm, RdFromAlu, NoMem, AluOpInfo(add4), BranchInfo("b11111".U))
            ),
            // --
            (inst === insts.beq) -> DecodeSignal(Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b00100".U))),
            (inst === insts.bne) -> DecodeSignal(Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b11011".U))),
            (inst === insts.blt) -> DecodeSignal(Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b10000".U))),
            (inst === insts.bge) -> DecodeSignal(Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b00110".U))),
            (inst === insts.bltu) -> DecodeSignal(
                Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b01000".U))
            ),
            (inst === insts.bgeu) -> DecodeSignal(
                Array(BType, AFromRs1, BFromRs2, NoRd, NoMem, BranchInfo("b00101".U))
            ),
            // --
            (inst === insts.lb)  -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.lbu) -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.lh)  -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.lhu) -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.lw) -> DecodeSignal(
                Array(IType, AFromRs1, BFromImm, RdFromMem, MemRead, AluOpInfo(add), Cut32)
            ),
            (inst === insts.lwu) -> DecodeSignal(Array(DecodeErr)), // No way to perform 0-ext (x
            (inst === insts.ld)  -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromMem, MemRead, AluOpInfo(add))),
            // --
            (inst === insts.sb) -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.sh) -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.sw) -> DecodeSignal(Array(DecodeErr)),
            (inst === insts.sd) -> DecodeSignal(Array(SType, AFromRs1, BFromImm, NoRd, MemWrite, AluOpInfo(add))),
            // --
            (inst === insts.addi)  -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(add))),
            (inst === insts.slti)  -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(slt))),
            (inst === insts.sltiu) -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(sltu))),
            (inst === insts.xori)  -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(xor))),
            (inst === insts.ori)   -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(or))),
            (inst === insts.andi)  -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(and))),
            (inst === insts.slli) -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(sll64))),
            (inst === insts.srli) -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(srl64))),
            (inst === insts.srai) -> DecodeSignal(Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(sra64))),
            // --
            (inst === insts.add)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(add))),
            (inst === insts.sub)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sub))),
            (inst === insts.slt)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(slt))),
            (inst === insts.sltu) -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sltu))),
            (inst === insts.xor)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(xor))),
            (inst === insts.or)   -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(or))),
            (inst === insts.and)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(and))),
            (inst === insts.sll)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sll64))),
            (inst === insts.srl)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(srl64))),
            (inst === insts.sra)  -> DecodeSignal(Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sra64))),
            // --
            (inst === insts.addiw) -> DecodeSignal(
                Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(add), Cut32)
            ),
            (inst === insts.slliw) -> DecodeSignal(
                Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(sll32), Cut32)
            ),
            (inst === insts.srliw) -> DecodeSignal(
                Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(srl32), Cut32)
            ),
            (inst === insts.sraiw) -> DecodeSignal(
                Array(IType, AFromRs1, BFromImm, RdFromAlu, NoMem, AluOpInfo(sra32), Cut32)
            ),
            // --
            (inst === insts.addw) -> DecodeSignal(
                Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(add), Cut32)
            ),
            (inst === insts.subw) -> DecodeSignal(
                Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sub), Cut32)
            ),
            (inst === insts.sllw) -> DecodeSignal(
                Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sll32), Cut32)
            ),
            (inst === insts.srlw) -> DecodeSignal(
                Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(srl32), Cut32)
            ),
            (inst === insts.sraw) -> DecodeSignal(
                Array(RType, AFromRs1, BFromRs2, RdFromAlu, NoMem, AluOpInfo(sra32), Cut32)
            )
        )
    )

    io.imm := 0.U
    switch(signals.instType) {
        is(IType.instType.U) { io.imm := Cat(Fill(52, inst(31)), inst(31, 20)) }
        is(SType.instType.U) { io.imm := Cat(Fill(52, inst(31)), inst(31, 25), inst(11, 7)) }
        is(BType.instType.U) { io.imm := Cat(Fill(52, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)) }
        is(UType.instType.U) { io.imm := Cat(Fill(32, inst(31)), inst(31, 12), 0.U(12.W)) }
        is(JType.instType.U) { io.imm := Cat(Fill(44, inst(31)), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)) }
    }

    io.sig := signals

}
