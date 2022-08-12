import chisel3._
import chisel3.util._
import scala.collection.mutable.ListBuffer
import scala.math.pow

class HalfAdder extends Module {
  val i_a = IO(Input(Bool()))
  val i_b = IO(Input(Bool()))
  val o_o = IO(Output(Bool()))
  val o_c = IO(Output(Bool()))
  o_o := i_a ^ i_b
  o_c := i_a & i_b
}

class FullAdder extends Module {
  val i_a = IO(Input(Bool()))
  val i_b = IO(Input(Bool()))
  val i_c = IO(Input(Bool()))
  val o_o = IO(Output(Bool()))
  val o_c = IO(Output(Bool()))
  val axb = i_a ^ i_b
  o_c := axb ^ i_c
  o_o := (i_a & i_b) | (i_c & axb)
}

// 4-bit multiplier using a Wallace Multiplier Tree
class WallaceMultiplier extends Module {
  val io = IO(new Bundle {
    val i_a = Input(UInt(4.W))
    val i_b = Input(UInt(4.W))
    val o_c = Output(UInt(8.W))
  })
  // Stage 0: Group inputs by weight
  // Weight 1:  a0.b0
  // Weight 2:  a0.b1 a1.b0
  // Weight 4:  a0.b2 a1.b1 a2.b0
  // Weight 8:  a0.b3 a1.b2 a2.b1 a3.b0
  // Weight 16: a1.b3 a2.b2 a3.b1
  // Weight 32: a2.b3 a3.b2
  // Weight 64: a3.b3
  val stage0 = ListBuffer[ListBuffer[Bool]]()
  // TODO Can I declare stage0 with N entries and then initialize them later?
  println("** Stage 0 **")
  println("Grouping partial products by weight:")
  for ( ppw <- 0 to 6 ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    print(s" - Weight ${pow(2, ppw)}: ")
    val weights = ListBuffer[Bool]() // All bits with a weight of 2**'ppw'
    for ( wa <- 0 to io.i_a.getWidth-1) { // 'wa': 2**wa is the weight of bit 'i_a(wa)'
      for ( wb <- 0 to io.i_b.getWidth-1) { // 'wb': same as 'wa'
        // Group Partial products by their weight 'ppw' 
        if (wa+wb == ppw) {
          print(s"a$wa.b$wb ")
          weights += io.i_a(wa) & io.i_b(wb)
        }
      }
    }
    println("")
    stage0 += weights
  }
  // TODO Verify the generator: Sanity check: if i_a.getWigth == 4 then stage0(3).length == 4

  println(stage0.getClass)

  // Stage 1: Reduce bits from stage 0 with a tree of half/full adders
  println("** Stage 1 **")
  println("Create arrays for the outputs")
  val stage1 = ListBuffer[ListBuffer[Bool]]()
  for (ppw <- 0 to 6) // Create a list of bits grouped by weight
    stage1.append(ListBuffer[Bool]())

  println("Wire Half/Full adders")
  for ( ppw <- 0 to 6 ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    val insize = stage0(ppw).length
    val next_weight = ppw+1
    print(s"$insize bits of weight ${pow(2, ppw)} from stage 0 => ")
    var remaining_bits = insize
    while (remaining_bits != 0) {
      if (remaining_bits >= 3) {
        print("FA ")
        val inst_fa = Module(new FullAdder())
        inst_fa.i_a := stage0(ppw)(remaining_bits-1)
        inst_fa.i_b := stage0(ppw)(remaining_bits-2)
        inst_fa.i_c := stage0(ppw)(remaining_bits-3)
        stage1(ppw).append(inst_fa.o_o)
        stage1(next_weight).append(inst_fa.o_c) // wire carry to the next weight
        remaining_bits -= 3 // A Full Adder "consumes" 3 inputs
      } else if (remaining_bits >= 2) {
        print("HA ")
        val inst_ha = Module(new HalfAdder())
        inst_ha.i_a := stage0(ppw)(remaining_bits-1)
        inst_ha.i_b := stage0(ppw)(remaining_bits-2)
        stage1(ppw).append(inst_ha.o_o)
        stage1(next_weight).append(inst_ha.o_c) // wire carry to the next weight
        remaining_bits -= 2 // A Half Adder "consumes" 2 inputs
      } else if (remaining_bits >= 2) {
      } else { // remaining_bits == 1
        // Directly wire the remaining bit to stage 1's outputs
        print("- ")
        stage1(ppw).append(stage0(ppw)(remaining_bits-1))
        remaining_bits -= 1 // consume 1 bit
      }
    }
    println("")
  }

  // Debugging prompt
  println("Stage 1's output:")
  var count = 0
  for (tt <- stage1) {
    println(s"Index $count (Weight ${pow(2, count)}): ${tt.length}-bit")
    count += 1
  }

  println("** Stage 2 **")
  println("Create arrays for the outputs")
  val stage2 = ListBuffer[ListBuffer[Bool]]()
  for (ppw <- 0 to 7) // Create a list of bits grouped by weight
    stage2.append(ListBuffer[Bool]())
  
  println("Wire Half/Full adders")
  for ( ppw <- 0 to 6 ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    val insize = stage1(ppw).length
    val next_weight = ppw+1
    print(s"$insize bits of weight ${pow(2, ppw)} from stage 1 => ")
    var remaining_bits = insize
    while (remaining_bits != 0) {
      if (remaining_bits >= 3) {
        print("FA ")
        val inst_fa = Module(new FullAdder())
        inst_fa.i_a := stage1(ppw)(remaining_bits-1)
        inst_fa.i_b := stage1(ppw)(remaining_bits-2)
        inst_fa.i_c := stage1(ppw)(remaining_bits-3)
        stage2(ppw).append(inst_fa.o_o)
        stage2(next_weight).append(inst_fa.o_c) // wire carry to the next weight
        remaining_bits -= 3 // A Full Adder "consumes" 3 inputs
      } else if (remaining_bits >= 2) {
        print("HA ")
        val inst_ha = Module(new HalfAdder())
        inst_ha.i_a := stage1(ppw)(remaining_bits-1)
        inst_ha.i_b := stage1(ppw)(remaining_bits-2)
        stage2(ppw).append(inst_ha.o_o)
        stage2(next_weight).append(inst_ha.o_c) // wire carry to the next weight
        remaining_bits -= 2 // A Half Adder "consumes" 2 inputs
      } else if (remaining_bits >= 2) {
      } else { // remaining_bits == 1
        // Directly wire the remaining bit to stage 1's outputs
        print("- ")
        stage2(ppw).append(stage1(ppw)(remaining_bits-1))
        remaining_bits -= 1 // consume 1 bit
      }
    }
    println("")
  }

  // Debugging prompt
  println("Stage 2's output:")
  count = 0
  for (tt <- stage2) {
    println(s"Index $count (Weight ${pow(2, count)}): ${tt.length}-bit")
    count += 1
  }

  println("** Stage 3 **")
  val ope_a = Wire(Vec(8, Bool())) // Operand A for the addition
  val ope_b = Wire(Vec(8, Bool())) // Operand B for the addition
  println("-> Wire operands")
  for (i <- 0 to 7) { // Bits of a certain weight
      ope_a(i) := stage2(i)(0).asBool
  }
  for (i <- 0 to 7) { // Bits of a certain weight
    if (stage2(i).length == 2) {
      ope_b(i) := stage2(i)(1).asBool
    } else if (stage2(i).length == 1) {
      ope_b(i) := 0.B 
    } else {
      println("Error: Too many/Too Few bits for the final addition, expecting 1 or 2 bits for each weight")
      throw new Exception("Generator error")
    }
  }

  println("-> Final addition")
  // Final addition without width expansion
  io.o_c := ope_a.asUInt +% ope_b.asUInt
}

// Generate the Verilog code
// To run in sbt:
// runMain WallaceMultiplierMain
object WallaceMultiplierMain extends App {
  println("Generating the Wallace Multiplier hardware")
  (new chisel3.stage.ChiselStage).emitVerilog(new WallaceMultiplier(), Array("--target-dir", "generated"))
}
