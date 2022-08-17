import org.scalatest._
import org.scalatest.flatspec._
import chiseltest._
import chisel3._
import math.pow
import scala.math.BigInt

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
  it should "compute max x max" in {
    test(new WallaceMultiplier()) { dut=>
      val r = scala.util.Random
      var a = scala.math.BigInt(0).setBit(dut.SIZE)-1
      var b = scala.math.BigInt(0).setBit(dut.SIZE)-1
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
  it should "compute 0 x max" in {
    test(new WallaceMultiplier()) { dut=>
      val r = scala.util.Random
      var a = scala.math.BigInt(0)
      var b = scala.math.BigInt(0).setBit(dut.SIZE)-1
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
  it should "compute 1 x max" in {
    test(new WallaceMultiplier()) { dut=>
      val r = scala.util.Random
      var a = scala.math.BigInt(1)
      var b = scala.math.BigInt(0).setBit(dut.SIZE)-1
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
  it should "compute max x 0" in {
    test(new WallaceMultiplier()) { dut=>
      val r = scala.util.Random
      var a = scala.math.BigInt(0).setBit(dut.SIZE)-1
      var b = scala.math.BigInt(0)
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
  it should "compute max x 1" in {
    test(new WallaceMultiplier()) { dut=>
      val r = scala.util.Random
      var a = scala.math.BigInt(0).setBit(dut.SIZE)-1
      var b = scala.math.BigInt(1)
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
  it should "pass random tests" in {
    test(new WallaceMultiplier()) { dut=>
      // Note: Scala Int is 32-bit and Long is 64-bit (signed)
      for (i <- 0 to 100) {
        val r = scala.util.Random
        var a = BigInt(dut.SIZE, r)
        var b = BigInt(dut.SIZE, r)
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
