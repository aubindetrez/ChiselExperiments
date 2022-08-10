import org.scalatest._
import org.scalatest.flatspec._
import chiseltest._
import chisel3._

class CounterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Counter"

  it should " counter from 0 to 200 " in {
    test(new Counter()) { dut =>
    /* test */
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
    /* 200 clock cycles */
    val count_value = dut.io.count.peek().litValue
    val count_value_next = dut.io.countNext.peek().litValue
    println(s"Initial count value: $count_value")
    println(s"Initial countNext value: $count_value_next")
    dut.clock.step(200)
    dut.io.count.expect(200.U)
    dut.io.countNext.expect(199.U)
    }
  }

  it should " be reinitialized to 0 after 200 " in { test(new Counter()) { dut =>
    /* test */
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
    /* 201 clock cycles */
    val count_value = dut.io.count.peek().litValue
    val count_value_next = dut.io.countNext.peek().litValue
    println(s"Initial count value: $count_value")
    println(s"Initial countNext value: $count_value_next")
    dut.clock.step(201)
    dut.io.count.expect(0.U)
    dut.io.countNext.expect(200.U)
    }
  }
}
