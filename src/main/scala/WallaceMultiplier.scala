import chisel3._
import chisel3.util._

// 4-bit multiplier using a Wallace Multiplier Tree
class WallaceMultiplier extends Module {
  val io = IO(new Bundle {
    val i_a = Input(UInt(4.W))
    val i_b = Input(UInt(4.W))
    val o_c = Output(UInt(8.W))
  })
  val stage0 = Vec(16, Wire(Bool()))
  // Create wires for stage 0
  // Weight 1:  a0.b0
  // Weight 2:  a0.b1 a1.b0
  // Weight 4:  a0.b2 a1.b1 a2.b0
  // Weight 8:  a0.b3 a1.b2 a2.b1 a3.b0
  // Weight 16: a1.b3 a2.b2 a3.b1
  // Weight 32: a2.b3 a3.b2
  // Weight 64: a3.b3

  // Create wires for stage 1

  // HA and FA for stage0 -> stage1

  // Create wires for stage 2

  // Final addition
  io.o_c := Cat(io.i_a, io.i_b)
}

// Generate the Verilog code
object WallaceMultiplierMain extends App {
  println("Generating the Wallace Multiplier hardware")
  (new chisel3.stage.ChiselStage).emitVerilog(new WallaceMultiplier(), Array("--target-dir", "generated"))
}
