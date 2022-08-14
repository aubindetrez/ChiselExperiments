import org.scalatest._
import org.scalatest.flatspec._
import chiseltest._
import chisel3._

// In `sbt` type: testOnly WallaceMultiplierSpec
class WallaceMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "WallaceMultiplier"

  it should "special case: 0x0" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(0.U)
      dut.io.i_b.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      println(s"0 times 0 is: $result")
      dut.io.o_c.expect(0.U)
    }
  }
  it should "special case: 1x1" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(1.U)
      dut.io.i_b.poke(1.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      println(s"1 times 1 is: $result")
      dut.io.o_c.expect(1.U)
    }
  }
  it should "special case: 2x2" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(2.U)
      dut.io.i_b.poke(2.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      println(s"2 times 2 is: $result")
      dut.io.o_c.expect(4.U)
    }
  }
  it should "special case: 15x1" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(15.U)
      dut.io.i_b.poke(1.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      println(s"15 times 1 is: $result")
      dut.io.o_c.expect(15.U)
    }
  }
  it should "special case: 1x15" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(1.U)
      dut.io.i_b.poke(15.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      println(s"1 times 15 is: $result")
      dut.io.o_c.expect(15.U)
    }
  }
}
