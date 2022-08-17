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


/** A 2-D array of bits used by the Wallace Multiplier
  * `DIM`: DIM(N) contains how many bits of weight 2**N have to be represented
  * [[
     // For a given 'DIM' parameter
     val DIM = TreeDim(1, 2, 3, 1)
     val list = DIM.map(nw => Vec(nw, Bool()))
     // Generates a List of Vec for each weight.
     // 1-bit for weight 1
     // 2-bit for weight 2
     // 3-bit for weight 4
     // 1-bit for weight 8
     // val list: Seq[chisel3.Vec[chisel3.Bool]] = List(Bool[1], Bool[2], Bool[3], Bool[1])
  * ]]
  */
class StageInterface(val DIM: WallaceMultiplier.TreeDim) extends Bundle {
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

  def toListDim : WallaceMultiplier.TreeDim = DIM

  // Take the first bit for every weight and wire them together to make a UInt
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

  // Take the second bit for every weight (or 1'b0) and wire them together to make a UInt
  def toOperantB : UInt = {
    val ope_b = Wire(Vec(this.length, Bool()))
    var i = 0
    for (bit <- list) {
      if (bit.length == 2) {
        ope_b(i) := bit(1).asBool
      } else if (bit.length == 1) {
        // No second bit for that weight -> use 1'b0
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
// Static functions for 'StageInterface'
object StageInterface {
  def connect(from: WallaceMultiplier.Mut2DBoolTree, to: StageInterface) = {
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
 *  it makes hierarchy easier to understand when debugging vcd traces
 *  `INDIM`: INDIM(N) contains how many bits of weight 2**N are in the input
 *  
 *  Example for the first reduce stage of a 4x4 multiplier
 *  Parameter:
 *   [[val DIM = TreeDim(1, 2, 3, 1)]]
 *  Input:
 *   MixedVec(Vec(a0b0.B), Vec(a0b1.B, a1b0.B), Vec(a0b2.B, a1b1.B, a2b0.B), ...
 *  Principle of operation:
 *  if for a given weight the input contains 2-bit -> feed them to a HalfAdder
 *  else if it contains 3-bit -> feed them to a FullAdder
 *  Output:
 *  MixedVec(Vec(a0b0.B), Vec(Result_HA(a0b1, a1b0)), Vec(Carry_HA(a0b1, a1b0), Result_FA(a0b2...
 */
class ReduceStage(val INDIM: WallaceMultiplier.TreeDim, val DEBUG: Boolean = false) extends Module {
  val i_prev_stage = IO(Input(new StageInterface(INDIM))) // Previous stage: a 2D weight matrix
  val INSIZE = i_prev_stage.length

  if (DEBUG) println("** Reduce Stage **")
  val current_stage = WallaceMultiplier.Mut2DBoolTree()

  // The output vector is 1-bit bigger than the input to accound for the carry bit if there is
  // more than 1 MSB bit
  val size_output = INSIZE + ( if (INDIM(INDIM.length-1) < 2) 0 else 1 )
  for (ppw <- 0 until size_output) // Create a list of bits grouped by weight
    current_stage.append(ListBuffer[Bool]())

  if (DEBUG) println("Wire Half/Full adders")
  for ( ppw <- 0 until INSIZE ) { // 'ppw': log2 of the partial prodcut weight: 1, 2, 4, 8, 16, 32, 64
    val insize = INDIM(ppw)
    val next_weight = ppw+1
    if (DEBUG) print(s"$insize bits of weight ${pow(2, ppw)} from the previous stage => ")
    var remaining_bits = insize
    while (remaining_bits != 0) {
      if (remaining_bits >= 3) {
        if (DEBUG) print("FA ")
        val inst_fa = Module(new FullAdder())
        inst_fa.i_a := i_prev_stage.getVecForWeight(ppw)(remaining_bits-1)
        inst_fa.i_b := i_prev_stage.getVecForWeight(ppw)(remaining_bits-2)
        inst_fa.i_c := i_prev_stage.getVecForWeight(ppw)(remaining_bits-3)
        current_stage(ppw).append(inst_fa.o_o)
        current_stage(next_weight).append(inst_fa.o_c) // wire carry to the next weight
        remaining_bits -= 3 // A Full Adder "consumes" 3 inputs
      } else if (remaining_bits >= 2) {
        if (DEBUG) print("HA ")
        val inst_ha = Module(new HalfAdder())
        inst_ha.i_a := i_prev_stage.getVecForWeight(ppw)(remaining_bits-1)
        inst_ha.i_b := i_prev_stage.getVecForWeight(ppw)(remaining_bits-2)
        current_stage(ppw).append(inst_ha.o_o)
        current_stage(next_weight).append(inst_ha.o_c) // wire carry to the next weight
        remaining_bits -= 2 // A Half Adder "consumes" 2 inputs
      } else if (remaining_bits >= 2) {
      } else { // remaining_bits == 1
        // Directly wire the remaining bit to stage 1's outputs
        if (DEBUG) print("- ")
        current_stage(ppw).append(i_prev_stage.getVecForWeight(ppw)(remaining_bits-1))
        remaining_bits -= 1 // consume 1 bit
      }
    }
    if (DEBUG) println("")
  }
  
  // Get Ouput's dimension
  val outdim = WallaceMultiplier.getListBufferDim(current_stage)

  val o_2dw = IO(Output(new StageInterface(outdim))) // 2D weight matrix

  // Convert mutable ListBuffer[Bool] to fixed-size Vec[Bool]
  if (DEBUG) println("ReduceStage: Wire output")
  for ( i <- 0 until current_stage.length) {
    if (DEBUG) print(s"Weight ${pow(2, i)}: ")
    val bools = current_stage(i)
    if (DEBUG) print(s"${bools.length}-bit")
    val w_2dw = Wire(Vec(bools.length, Bool()))
    assert (w_2dw.length == bools.length)
    for (j <- 0 until bools.length) {
      w_2dw(j) := bools(j)
    }
    o_2dw.setWeight(i, w_2dw)
    if (DEBUG) println()
  }
}

// multiplier using a Wallace Multiplier Tree
class WallaceMultiplier(val SIZE: Int = 8, val DEBUG: Boolean = false) extends Module {
  val OUTSIZE = 2*SIZE
  val io = IO(new Bundle {
    val i_a = Input(UInt(SIZE.W))
    val i_b = Input(UInt(SIZE.W))
    val o_c = Output(UInt(OUTSIZE.W))
  })
  // Stage 0: Group inputs by weight
  // Weight 1:  a0.b0
  // Weight 2:  a0.b1 a1.b0
  // Weight 4:  a0.b2 a1.b1 a2.b0
  // Weight 8:  a0.b3 a1.b2 a2.b1 a3.b0
  // Weight 16: a1.b3 a2.b2 a3.b1
  // Weight 32: a2.b3 a3.b2
  // Weight 64: a3.b3
  val STAGE0_SIZE = 2*SIZE-1
  val stage0 = WallaceMultiplier.Mut2DBoolTree() // It is easier to use a mutable list
  for (ppw <- 0 until STAGE0_SIZE) // Create a list of bits grouped by weight
    stage0.append(ListBuffer[Bool]())

  if (DEBUG) {
    println("** Stage 0 **")
    println("Grouping partial products by weight:")
  }
  for ( ppw <- 0 until STAGE0_SIZE) {
    if (DEBUG) print(s" - Weight ${pow(2, ppw)}: ")
    for ( wa <- 0 to io.i_a.getWidth-1) { // 'wa': 2**wa is the weight of bit 'i_a(wa)'
      for ( wb <- 0 to io.i_b.getWidth-1) { // 'wb': same as 'wa'
        // Group Partial products by their weight
        if (wa+wb == ppw) {
          if (DEBUG) print(s"a$wa.b$wb ")
          stage0(ppw).append(io.i_a(wa) & io.i_b(wb))
        }
      }
    }
    if (DEBUG) println("")
  }
  // TODO formal
  // TODO Verify the generator: Sanity check: if i_a.getWigth == 4 then stage0(3).length == 4

  // Stage 1: Reduce
  // Weight 1:  a0.b0
  // Weight 2:  HA(a0.b1 a1.b0) [carry is of weight 4]
  // Weight 4:  FA(a0.b2 a1.b1 a2.b0) [carry is of weight 8]
  // Weight 8:  FA(a0.b3 a1.b2 a2.b1) a3.b0 [carry is of weight 16]
  // Weight 16: FA(a1.b3 a2.b2 a3.b1) [carry is of weight 32]
  // Weight 32: HA(a2.b3 a3.b2) [carry is of weight 64]
  // Weight 64: a3.b3

  // Stage 2: Reduce
  // Weight 1:  a0.b0
  // Weight 2:  Res
  // Weight 4:  HA(Carry Res)
  // Weight 8:  FA(Carry Res a3.b0)
  // Weight 16: HA(Carry Res)
  // Weight 32: HA(Carry Res)
  // Weight 64: HA(Carry a3.b3)
  
  val stage0_int = Wire(new StageInterface(WallaceMultiplier.getListBufferDim(stage0)))
  StageInterface.connect(stage0, stage0_int)

  var stage_count = 1
  var dim = WallaceMultiplier.getListBufferDim(stage0)
  var reduce_input: StageInterface = stage0_int
  var is_stage_final = false
  // Generate an arbitrary number of stage until the tree only contains 1 or 2-bit per weight
  while (!is_stage_final) {
    if (DEBUG) println(s"**STAGE $stage_count**")
    val inst_reduce_stage = Module(new ReduceStage(dim, DEBUG))
    StageInterface.connect(reduce_input, inst_reduce_stage.i_prev_stage)
    dim = inst_reduce_stage.o_2dw.toListDim
    reduce_input = inst_reduce_stage.o_2dw
    is_stage_final = inst_reduce_stage.o_2dw.isFinal
    stage_count += 1
  }

  // Stage 3: Final Addition
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
  if (DEBUG) println("** Final Stage: addition **")
  // Final addition without width expansion
  io.o_c := reduce_input.toOperantA +% reduce_input.toOperantB
}

object WallaceMultiplier {
  type Mut2DBoolTree = ListBuffer[ListBuffer[Bool]]
  def Mut2DBoolTree(xs: ListBuffer[Bool]*) = ListBuffer(xs: _*)

  type TreeDim = Seq[Int]
  def TreeDim(xs: Int*) = Seq(xs: _*)

  // Convert 2D Boolean Tree to a List of Length (of each column)
  def getListBufferDim (in: WallaceMultiplier.Mut2DBoolTree) : WallaceMultiplier.TreeDim = Seq.tabulate(in.length)(n => in(n).length)
}

// Generate the Verilog code
// To run in sbt:
// runMain WallaceMultiplierMain
object WallaceMultiplierMain extends App {
  println("Generating the Wallace Multiplier hardware")
  (new chisel3.stage.ChiselStage).emitVerilog(new WallaceMultiplier(DEBUG=true), Array("--target-dir", "generated"))
}
