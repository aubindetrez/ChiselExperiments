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
  o_o := axb ^ i_c
  o_c := (i_a & i_b) | (i_c & axb)
}


/**
 * `DIM`: DIM(0) contains how many bits of weight 1 (2**1) is in the input
 */
class StageInterface(val DIM: Seq[Int]) extends Bundle {
  // scala> val DIM: Seq[Int] = Seq(1, 2, 3, 1)
  // scala> val list = DIM.map(nw => Vec(nw, Bool()))
  // val list: Seq[chisel3.Vec[chisel3.Bool]] = List(Bool[1], Bool[2], Bool[3], Bool[1])
  // Vec can only hold the same type / width -> Use MixedVec instead
  val list: chisel3.util.MixedVec[chisel3.Vec[chisel3.Bool]] = MixedVec( DIM.map(nw => Vec(nw, Bool())) )
 
  def length = list.length
  def getVecForWeight(weight: Int) : Vec[Bool] = list(weight)
  def setWeight(i: Int, vec: Vec[Bool]) = list(i) := vec
  // Return true if there is 1 or 2 bits per weight
  def isFinal : Boolean = {
    for (vec <- list) {
      if (vec.length > 2) return false
    }
    return true
  }
  def toListDim : Seq[Int] = DIM
  def toOperantA : UInt = {
    val ope_a = Wire(Vec(this.length, Bool()))
    var i = 0
    for (bit <- list) {
      if (bit.length > 0) {
        ope_a(i) := bit(0).asBool // bit.length is always > 0
      } else {
        println(s"Error: No bit for weight ${pow(2, i)} => Cannot create operandA")
        throw new Exception("Generator error")
      }
      i += 1
    }
    return ope_a.asUInt
  }
  def toOperantB : UInt = {
    val ope_b = Wire(Vec(this.length, Bool()))
    var i = 0
    for (bit <- list) {
      if (bit.length == 2) {
        ope_b(i) := bit(1).asBool
      } else if (bit.length == 1) {
        ope_b(i) := 0.B
      } else {
        println("Error: Too many/Too Few bits for the final addition, expecting 1 or 2 bits for each weight")
        throw new Exception("Generator error")
      }
      i += 1
    }
    return ope_b.asUInt
  }
}
object StageInterface {
  def connect(from: ListBuffer[ListBuffer[Bool]], to: StageInterface) = {
    if ( from.length != to.length ) {
        println("Error: Try to connect Two stages with different size")
        throw new Exception("Cannot connect 2 stages of different size")
    }
    // Wire 'from' to 'to' (StageInterface)
    for (i <- 0 until from.length) {
      if (from(i).length != to.getVecForWeight(i).length) {
        println("Error: Try to connect Two stages with different size")
        throw new Exception("Cannot connect 2 stages of different size")
      }
      for (j <- 0 until from(i).length) {
        to.getVecForWeight(i)(j) := from(i)(j)
      }
    }
  }
  def connect(from: StageInterface, to: StageInterface) = {
    if ( from.length != to.length ) {
        println("Error: Try to connect Two stages with different size")
        throw new Exception("Cannot connect 2 stages of different size")
    }

    for (i <- 0 until from.length) {
      val a = from.getVecForWeight(i) // Vec[Bool]
      val b = to.getVecForWeight(i)
      if (a.length != b.length) {
        println(s"Error: Trying to connect two stages but number of bits for weight ${pow(2, i)} do not match")
        throw new Exception("Cannot connect 2 stages of different size")
      }
      b := a // Wire stages together
    }
  }
}

/** Reduce Stages are in a separate module,
 * it makes hierarchy easier to understand when debugging vcd traces
 * `INSIZE`: How many bits of different weight are in the input
 * `INDIM`: INDIM(0) contains how many bits of weight 1 (2**0) are in the input
 * INDIM(1) contains how many bits of weight 2(2**1) are in the input
 */
class ReduceStage(val INDIM: Seq[Int]) extends Module {
  val i_prev_stage = IO(Input(new StageInterface(INDIM))) // Previous stage: a 2D weight matrix

  println("** Reduce Stage **")
  val current_stage = ListBuffer[ListBuffer[Bool]]()

  // Accound for the carry bit if there is more than 1 MSB bit
  val size_output = INDIM.length + ( if (INDIM(INDIM.length-1) < 2) 0 else 1 )
  for (ppw <- 0 until size_output) // Create a list of bits grouped by weight
    current_stage.append(ListBuffer[Bool]())

  println("Wire Half/Full adders")
  for ( ppw <- 0 to 6 ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    val insize = INDIM(ppw)
    val next_weight = ppw+1
    print(s"$insize bits of weight ${pow(2, ppw)} from stage 0 => ")
    var remaining_bits = insize
    while (remaining_bits != 0) {
      if (remaining_bits >= 3) {
        print("FA ")
        val inst_fa = Module(new FullAdder())
        inst_fa.i_a := i_prev_stage.getVecForWeight(ppw)(remaining_bits-1)
        inst_fa.i_b := i_prev_stage.getVecForWeight(ppw)(remaining_bits-2)
        inst_fa.i_c := i_prev_stage.getVecForWeight(ppw)(remaining_bits-3)
        current_stage(ppw).append(inst_fa.o_o)
        current_stage(next_weight).append(inst_fa.o_c) // wire carry to the next weight
        remaining_bits -= 3 // A Full Adder "consumes" 3 inputs
      } else if (remaining_bits >= 2) {
        print("HA ")
        val inst_ha = Module(new HalfAdder())
        inst_ha.i_a := i_prev_stage.getVecForWeight(ppw)(remaining_bits-1)
        inst_ha.i_b := i_prev_stage.getVecForWeight(ppw)(remaining_bits-2)
        current_stage(ppw).append(inst_ha.o_o)
        current_stage(next_weight).append(inst_ha.o_c) // wire carry to the next weight
        remaining_bits -= 2 // A Half Adder "consumes" 2 inputs
      } else if (remaining_bits >= 2) {
      } else { // remaining_bits == 1
        // Directly wire the remaining bit to stage 1's outputs
        print("- ")
        current_stage(ppw).append(i_prev_stage.getVecForWeight(ppw)(remaining_bits-1))
        remaining_bits -= 1 // consume 1 bit
      }
    }
    println("")
  }
  
  // Get Ouput's dimension
  val outdim = WallaceMultiplier.list2listlength(current_stage)
  //val outdim = ListBuffer[Int]()
  //for (bits <- current_stage) {
  //  outdim.append(bits.length)
  //}

  val o_2dw = IO(Output(new StageInterface(outdim))) // 2D weight matrix

  // Convert mutable ListBuffer[Bool] to fixed-size Vec[Bool]
  println("ReduceStage: Wire output")
  for ( i <- 0 until current_stage.length) {
    print(s"Weight ${pow(2, i)}: ")
    val bools = current_stage(i)
    print(s"${bools.length}-bit")
    val w_2dw = Wire(Vec(bools.length, Bool()))
    assert (w_2dw.length == bools.length)
    for (j <- 0 until bools.length) {
      w_2dw(j) := bools(j)
    }
    o_2dw.setWeight(i, w_2dw)
    println()
  }
}

// 4-bit multiplier using a Wallace Multiplier Tree
class WallaceMultiplier(val SIZE: Int = 4) extends Module {
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
  val stage0 = ListBuffer[ListBuffer[Bool]]() // It is easier to use a mutable list
  for (ppw <- 0 to 6) // Create a list of bits grouped by weight
    stage0.append(ListBuffer[Bool]())
  println("** Stage 0 **")
  println("Grouping partial products by weight:")
  for ( ppw <- 0 to 6 ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    print(s" - Weight ${pow(2, ppw)}: ")
    for ( wa <- 0 to io.i_a.getWidth-1) { // 'wa': 2**wa is the weight of bit 'i_a(wa)'
      for ( wb <- 0 to io.i_b.getWidth-1) { // 'wb': same as 'wa'
        // Group Partial products by their weight 'ppw' 
        if (wa+wb == ppw) {
          print(s"a$wa.b$wb ")
          stage0(ppw).append(io.i_a(wa) & io.i_b(wb))
        }
      }
    }
    println("")
  }
  // TODO formal
  // TODO Verify the generator: Sanity check: if i_a.getWigth == 4 then stage0(3).length == 4
  println(stage0.getClass)

  // Stage 1: Reduce
  // Weight 1:  a0.b0
  // Weight 2:  HA(a0.b1 a1.b0) [carry is of weight 4]
  // Weight 4:  FA(a0.b2 a1.b1 a2.b0) [carry is of weight 8]
  // Weight 8:  FA(a0.b3 a1.b2 a2.b1) a3.b0 [carry is of weight 16]
  // Weight 16: FA(a1.b3 a2.b2 a3.b1) [carry is of weight 32]
  // Weight 32: HA(a2.b3 a3.b2) [carry is of weight 64]
  // Weight 64: a3.b3
  val inst_stage1 = Module(new ReduceStage(WallaceMultiplier.list2listlength(stage0)))
  StageInterface.connect(stage0, inst_stage1.i_prev_stage)

  // Stage 2: Reduce
  // Weight 1:  a0.b0
  // Weight 2:  Res
  // Weight 4:  HA(Carry Res)
  // Weight 8:  FA(Carry Res a3.b0)
  // Weight 16: HA(Carry Res)
  // Weight 32: HA(Carry Res)
  // Weight 64: HA(Carry a3.b3)
  val inst_stage2 = Module(new ReduceStage(inst_stage1.o_2dw.toListDim))
  StageInterface.connect(inst_stage1.o_2dw, inst_stage2.i_prev_stage)

  assert (inst_stage2.o_2dw.isFinal)

  // Stage 3: Addition
  //              A      B
  // Weight 1:   a0.b0   0
  // Weight 2:   Res     0
  // Weight 4:   Res     0
  // Weight 8:   Carry   Res
  // Weight 16:  Carry   Res
  // Weight 32:  Carry   Res
  // Weight 64:  Carry   Res
  // Weight 128: Carry   0
  // TODO Use a smaller adder and ignore bits of weight 1..4
  println("-> Final addition")
  // Final addition without width expansion
  io.o_c := inst_stage2.o_2dw.toOperantA +% inst_stage2.o_2dw.toOperantB
}

object WallaceMultiplier {
  // Convert 2D ListBuffer to a List of Length (of each column)
  def list2listlength (in: ListBuffer[ListBuffer[Bool]]) : Seq[Int] = Seq.tabulate(in.length)(n => in(n).length)
}

// Generate the Verilog code
// To run in sbt:
// runMain WallaceMultiplierMain
object WallaceMultiplierMain extends App {
  println("Generating the Wallace Multiplier hardware")
  (new chisel3.stage.ChiselStage).emitVerilog(new WallaceMultiplier(), Array("--target-dir", "generated"))
}
