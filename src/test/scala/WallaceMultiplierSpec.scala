import org.scalatest._
import org.scalatest.flatspec._
import chiseltest._
import chisel3._
import math.pow

// In `sbt` type: testOnly WallaceMultiplierSpec
// testOnly WallaceMultiplierSpec -- -DwriteVcd=1
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
      if (WallaceMultiplierSpec.isDebug) {
        println(s"0 times 0 is: $result")
      }
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
      if (WallaceMultiplierSpec.isDebug) {
        println(s"1 times 1 is: $result")
      }
      dut.io.o_c.expect(1.U)
    }
  }
  it should "special case: 2x2" in {
    test(new WallaceMultiplier()).withAnnotations(Seq(WriteVcdAnnotation)) { dut=>
      dut.io.i_a.poke(2.U)
      dut.io.i_b.poke(2.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      if (WallaceMultiplierSpec.isDebug) {
        println(s"2 times 2 is: $result")
      }
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
      if (WallaceMultiplierSpec.isDebug) {
        println(s"15 times 1 is: $result")
      }
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
      if (WallaceMultiplierSpec.isDebug) {
        println(s"1 times 15 is: $result")
      }
      dut.io.o_c.expect(15.U)
    }
  }
  it should "special case: 15x15" in {
    test(new WallaceMultiplier()) { dut=>
      dut.io.i_a.poke(15.U)
      dut.io.i_b.poke(15.U)
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      val result = dut.io.o_c.peek().litValue
      if (WallaceMultiplierSpec.isDebug) {
        println(s"15 times 15 is (expects 225): $result")
      }
      dut.io.o_c.expect(225.U)
    }
  }
  it should "pass random tests" in {
    test(new WallaceMultiplier()) { dut=>
      // FIXME: Warning: Scala int is 32-bit
      for (i <- 0 to 100) {
        val r = scala.util.Random
        var a = r.nextInt(pow(2, dut.SIZE).toInt)
        var b = r.nextInt(pow(2, dut.SIZE).toInt)
        dut.io.i_a.poke(a)
        dut.io.i_b.poke(b)
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)
        val result = dut.io.o_c.peek().litValue
        if (WallaceMultiplierSpec.isDebug) {
          println(s"$a times $b is (expects ${a*b}): $result")
        }
        dut.io.o_c.expect(a*b)
      }
    }
  }
}

object WallaceMultiplierSpec {
  def isDebug = false // Used to turn off/on debugging
}
