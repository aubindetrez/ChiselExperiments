import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class EdgeCounter(COUNTER_WIDTH: Int = 16) extends Module {
  val io = IO(new Bundle {
    val count = Output(UInt(COUNTER_WIDTH.W))
    val sig = Input(Bool())
  })

  val counter = RegInit(0.U(COUNTER_WIDTH.W))
  val old_sig = RegInit(true.B)
  val new_sig = RegInit(true.B)

  new_sig := io.sig
  old_sig := new_sig

  counter := Mux(!old_sig & new_sig, counter + 1.U, counter)

  io.count := counter
}

object EdgeCounterDriver extends App {
  println("Generate EdgeCounter verilog")
  (new ChiselStage).emitVerilog(new EdgeCounter())
}
