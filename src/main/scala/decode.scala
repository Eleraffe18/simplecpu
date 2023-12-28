package simplecpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental._

import Defs._

object InstType extends Enumeration {
    val i = 0
    val r = 1
    val s = 2
    var b = 3
    val u = 4
    val j = 5
    def apply(v: Int)(x: DecodeSignal) = {
        x.instType := v.U
    }
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
    def apply(param: Array[(DecodeSignal) => Unit]) = {
        val sig = Wire(new DecodeSignal)
        sig := 0.U.asTypeOf(new DecodeSignal)
        param.foreach((nparam) => {
            nparam(sig)
        })
        sig
    }
}

class DecodeIO extends Bundle {
    val inst = Input(UInt(32.W))
    val imm  = Output(UInt(DW.W))
    val sig  = Output(new DecodeSignal)
}

object Decode {
    def oprandA(s: String)(x: DecodeSignal) = {
        x.asel := (s match {
            case "rs1" => 0.U
            case _ => 1.U
        })
    }
    def oprandB(s: String)(x: DecodeSignal) = {
        x.bsel := (s match {
            case "rs2" => 0.U
            case _ => 1.U
        })
    }
    def result(s: String)(x: DecodeSignal) = {
        x.rdWe := true.B
        x.osel := (s match {
            case "alu" => 0.U
            case _ => 1.U
        })
    }
    def mem(s: String)(x: DecodeSignal) = {
        x.memEn := true.B
        x.memWe := (s match {
            case "write" => true.B
            case _ => false.B
        })
    }
    def alu(s: UInt)(x: DecodeSignal) = {
        x.aluop := s
    }
    def branch(s: UInt)(x: DecodeSignal) = {
        x.brCond := s
    }
    def c32(x: DecodeSignal) = {
        x.cut32 := true.B
    }
    def instIsJal(x: DecodeSignal) = {
        x.isJal := true.B
    }
    def err(x: DecodeSignal) = {
        x.err := true.B
    }
    
}

class Decode extends Module {

    val io = IO(new DecodeIO)

    val signals = Wire(new DecodeSignal)
    val inst    = io.inst

    import AluOpcode._
    import Decode._

    object insts extends simplecpu.inst.RV64I

    val F = false.B
    val T = true.B

    signals := MuxCase(
        DecodeSignal(Array(err)),
        Seq(
            (inst === insts.lui)   -> DecodeSignal(Array(InstType(InstType.u), oprandA("rs1"), oprandB("imm"), result("alu"), alu(b))),
            (inst === insts.auipc) -> DecodeSignal(Array(InstType(InstType.u), oprandA("pc"), oprandB("imm"), result("alu"), alu(add))),
            // --
            (inst === insts.jal) -> DecodeSignal(
                Array(InstType(InstType.j), oprandA("pc"), oprandB("imm"), result("alu"), alu(add4), instIsJal, branch("b11111".U))
            ),
            (inst === insts.jalr) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("pc"), oprandB("imm"), result("alu"), alu(add4), branch("b11111".U))
            ),
            // --
            (inst === insts.beq) -> DecodeSignal(Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b00100".U))),
            (inst === insts.bne) -> DecodeSignal(Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b11011".U))),
            (inst === insts.blt) -> DecodeSignal(Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b10000".U))),
            (inst === insts.bge) -> DecodeSignal(Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b00110".U))),
            (inst === insts.bltu) -> DecodeSignal(
                Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b01000".U))
            ),
            (inst === insts.bgeu) -> DecodeSignal(
                Array(InstType(InstType.b), oprandA("rs1"), oprandB("rs2"), branch("b00101".U))
            ),
            // --
            (inst === insts.lb)  -> DecodeSignal(Array(err)),
            (inst === insts.lbu) -> DecodeSignal(Array(err)),
            (inst === insts.lh)  -> DecodeSignal(Array(err)),
            (inst === insts.lhu) -> DecodeSignal(Array(err)),
            (inst === insts.lw) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("mem"), mem("read"), alu(add), c32)
            ),
            (inst === insts.lwu) -> DecodeSignal(Array(err)), // No way to perform 0-ext (x
            (inst === insts.ld)  -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("mem"), mem("read"), alu(add))),
            // --
            (inst === insts.sb) -> DecodeSignal(Array(err)),
            (inst === insts.sh) -> DecodeSignal(Array(err)),
            (inst === insts.sw) -> DecodeSignal(Array(err)),
            (inst === insts.sd) -> DecodeSignal(Array(InstType(InstType.s), oprandA("rs1"), oprandB("imm"), mem("write"), alu(add))),
            // --
            (inst === insts.addi)  -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(add))),
            (inst === insts.slti)  -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(slt))),
            (inst === insts.sltiu) -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(sltu))),
            (inst === insts.xori)  -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(xor))),
            (inst === insts.ori)   -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(or))),
            (inst === insts.andi)  -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(and))),
            (inst === insts.slli) -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(sll64))),
            (inst === insts.srli) -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(srl64))),
            (inst === insts.srai) -> DecodeSignal(Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(sra64))),
            // --
            (inst === insts.add)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(add))),
            (inst === insts.sub)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sub))),
            (inst === insts.slt)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(slt))),
            (inst === insts.sltu) -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sltu))),
            (inst === insts.xor)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(xor))),
            (inst === insts.or)   -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(or))),
            (inst === insts.and)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(and))),
            (inst === insts.sll)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sll64))),
            (inst === insts.srl)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(srl64))),
            (inst === insts.sra)  -> DecodeSignal(Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sra64))),
            // --
            (inst === insts.addiw) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(add), c32)
            ),
            (inst === insts.slliw) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(sll32), c32)
            ),
            (inst === insts.srliw) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(srl32), c32)
            ),
            (inst === insts.sraiw) -> DecodeSignal(
                Array(InstType(InstType.i), oprandA("rs1"), oprandB("imm"), result("alu"), alu(sra32), c32)
            ),
            // --
            (inst === insts.addw) -> DecodeSignal(
                Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(add), c32)
            ),
            (inst === insts.subw) -> DecodeSignal(
                Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sub), c32)
            ),
            (inst === insts.sllw) -> DecodeSignal(
                Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sll32), c32)
            ),
            (inst === insts.srlw) -> DecodeSignal(
                Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(srl32), c32)
            ),
            (inst === insts.sraw) -> DecodeSignal(
                Array(InstType(InstType.r), oprandA("rs1"), oprandB("rs2"), result("alu"), alu(sra32), c32)
            )
            
        )
    )

    io.imm := 0.U

    {
    import InstType._
        switch(signals.instType) {
            is(i.U) { io.imm := Cat(Fill(52, inst(31)), inst(31, 20)) }
            is(s.U) { io.imm := Cat(Fill(52, inst(31)), inst(31, 25), inst(11, 7)) }
            is(b.U) { io.imm := Cat(Fill(52, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)) }
            is(u.U) { io.imm := Cat(Fill(32, inst(31)), inst(31, 12), 0.U(12.W)) }
            is(j.U) { io.imm := Cat(Fill(44, inst(31)), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)) }
        }
    }

    io.sig := signals

}
