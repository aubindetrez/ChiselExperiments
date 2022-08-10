import org.scalatest._
import org.scalatest.flatspec._
import chiseltest._
import chisel3._

class EdgeCounterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "EdgeCounter"

  it should "start at 0" in {
    test(new EdgeCounter()) { dut=>
      dut.io.sig.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val counter_value = dut.io.count.peek().litValue
      println(s"Counter value after reset: $counter_value")
      dut.io.count.expect(0.U)
    }
  }

  it should "increment on the raising edge of input (start high)" in {
    test(new EdgeCounter()) { dut=>
      dut.io.sig.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val counter_value = dut.io.count.peek().litValue
      println(s"Counter value after reset: $counter_value")
      dut.io.count.expect(0.U)

      dut.io.sig.poke(true.B)
      dut.clock.step(1)
      dut.io.count.expect(0.U)

      dut.io.sig.poke(false.B)
      dut.clock.step(2)
      dut.io.count.expect(0.U)

      dut.io.sig.poke(true.B)
      dut.clock.step(2)
      dut.io.count.expect(1.U)

      dut.io.sig.poke(false.B)
      dut.clock.step(1)
      dut.io.sig.poke(true.B)
      dut.clock.step(1)

      dut.io.sig.poke(false.B)
      dut.clock.step(1)

      dut.io.count.expect(2.U) // 1 cycle latency
      dut.io.sig.poke(true.B)
      dut.clock.step(1)

      dut.clock.step(1)
    }
  }

  it should "overflow to 0 after 2**COUNTER_WIDTH edges" in {
    test(new EdgeCounter()) { dut=>
    }
  }
}
