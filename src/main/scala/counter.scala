import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

object Hello {
  def main(args: Array[String]) : Unit = {
    println("Hello, World!")
  }
}

trait DisplayCounterParam {
  val size: Int;
  def display(): Unit = {
    println("Size = " + size)
  }
}

case class CounterParam(size: Int = 8) extends DisplayCounterParam

/*
 * Bool
 * Bits
 * UInt
 * SInt
 *
 * 4.U(10.W) // 10 bits
 * "h0f".U(4.W) // 4 bits
 * -3.S
 * true.B
 *
 * Input()
 * Output()
 * Analog()
 *
 * RawModule to specify clock and reset
 */
class Counter extends Module {
  val io = IO(new Bundle {
    val count = Output(UInt(8.W))
    val countNext = Output(UInt(8.W))
  })

  val countWire = Wire(UInt(8.W))
  val countReg = RegInit(0.U(8.W))

  when(countReg >= 200.U) {
    countWire := 0.U
  }
  // .elsewhen(countWire == 100.U) {}
  .otherwise{
    countWire := countReg + 1.U
  }
  //countWire := Mux(countWire >= 200.U, 0.U, countWire + 1.U)
  
  countReg := countWire
  
  // & | ~
  io.count := countReg
  io.countNext := RegNext(countReg)
}

object CounterDriver extends App {
  println("Generate Counter verilog")
  (new ChiselStage).emitVerilog(new Counter())
}

class PriorityLogic extends Module {
  val io = IO(new Bundle {
    val i = Input(Bits(1.W))
    val o = Output(Bits(1.W))
  })
  val a = Reg(Bits(1.W))
  val b = Reg(Bits(1.W))
  val c = Reg(Bits(1.W))

  io.o := c
  c := b
  b := a
  a := io.i
}

object PriorityLogicDriver extends App {
  println("Generate PriorityLogic verilog")
  (new ChiselStage).emitVerilog(new PriorityLogic())
}
