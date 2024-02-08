
package naxriscv.platform.asic
import naxriscv.compatibility.MultiPortWritesSymplifier
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.internals.{MemTopology, PhaseContext, PhaseMemBlackBoxingWithPolicy, PhaseNetlist}
import spinal.lib._

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SpinalSky130{
  object blackboxPolicy extends MemBlackboxingPolicy {
    override def translationInterest(topology: MemTopology): Boolean = topology.readsSync.nonEmpty || topology.readWriteSync.nonEmpty
    override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = {}
  }
  def apply() = {
    val c = SpinalConfig(mode = Verilog)
//    c.addTransformationPhase(new MultiPortWritesSymplifier)
    c.addStandardMemBlackboxing(blackboxPolicy)
    c.phasesInserters += { phases =>
      val i = phases.lastIndexWhere(_.isInstanceOf[PhaseMemBlackBoxingWithPolicy])
      phases.insert(i+1, new OpenRamPhase)
    }

    c
  }
}

/**
 * Manual steps :
 * - *.lef from openram patch => DATABASE MICRONS 1000 ;
 * - Openram technology/sky130/tech/tech.py =>
 *   spice["nand3_leakage"] = 1       # Leakage power of 3-input nand in nW
 *   + spice["nand4_leakage"] = 1       # Leakage power of 4-input nand in nW
 *   spice["nor2_leakage"] = 1        # Leakage power of 2-input nor in nW
 */
class OpenRamPhase extends PhaseNetlist {
  override def impl(pc: PhaseContext): Unit = {

    case class Config_1r1w(name : String, wordCount : Int, dataWidth : Int, bitPerMask : Option[Int])
    val configs_1r1w = mutable.LinkedHashSet[Config_1r1w]()
    pc.walkComponents {
      case bb: Ram_1w_1rs => bb.parent.rework {
        val skyWords = bb.wordCount
        val skyDataWidth = bb.wordWidth
        val skyMaskWidth = bb.wrMaskWidth
        val skyBitPerMask = bb.wrDataWidth/bb.wrMaskWidth
        val skyMask = if(bb.wrMaskEnable) s"_$skyBitPerMask" else ""
        val name = s"sky130_sram_1r1w_${skyDataWidth}x${skyWords}$skyMask"

        configs_1r1w += Config_1r1w(name, skyWords, skyDataWidth, if(bb.wrMaskEnable) Some(skyBitPerMask) else Option.empty[Int])


        bb.setDefinitionName(name)
        bb.genericElements.clear()
        bb.io.wr.clk.setName("clk0")
        bb.io.wr.addr.setName("addr0")
        bb.io.wr.data.setName("din0")

        bb.io.wr.en.setName("csb0")
        val wrEn = bb.io.wr.en.getSingleDriver.get
        bb.io.wr.en.removeAssignments() := !wrEn

        bb.wrMaskEnable match {
          case false => {
            bb.io.wr.mask.removeAssignments()
            bb.io.wr.mask.removeStatement()
          }
          case true => {
            bb.io.wr.mask.setName("wmask0")
            bb.io.wr.mask.setWidth(skyMaskWidth)
          }
        }

        bb.io.rd.clk.setName("clk1")
        bb.io.rd.addr.setName("addr1")
        bb.io.rd.data.setName("dout1")
        bb.io.rd.en.setName("csb1")
        val rdEn = bb.io.rd.en.getSingleDriver.get
        bb.io.rd.en.removeAssignments() := !rdEn
      }
      case _ =>
    }

    val macros = ArrayBuffer[String]()
    println("Generate openram macros")


    for(c <- configs_1r1w){
      macros += c.name
      val dir = new File(s"sram")
      FileUtils.forceMkdir(dir)

      val bf = new BufferedWriter(new FileWriter(new File(dir, c.name + ".py")))
      val mask = c.bitPerMask match {
        case Some(x) => s"write_size = $x"
        case None => ""
      }
      bf.write(
        s"""word_size = ${c.dataWidth} # Bits
           |num_words = ${c.wordCount}
           |
           |# Dual port
           |num_rw_ports = 0
           |num_r_ports = 1
           |num_w_ports = 1
           |ports_human = '1r1w'
           |
           |$mask
           |
           |tech_name = "sky130"
           |nominal_corner_only = True
           |
           |route_supplies = "ring"
           |#route_supplies = "left"
           |check_lvsdrc = True
           |uniquify = True
           |#perimeter_pins = False
           |#netlist_only = True
           |#analytical_delay = False
           |
           |output_name = "${c.name}"
           |output_path = "macro/{output_name}".format(**locals())
           |
           |check_lvsdrc = False
           |""".stripMargin)

      bf.close()

      val sbf = new BufferedWriter(new FileWriter(new File(dir, c.name + "_sc.py")))
      sbf.write(
        s"""import siliconcompiler
           |
           |def setup(chip):
           |    # Core values.
           |    design = '${c.name}'
           |    stackup = chip.get('option', 'stackup')
           |
           |    # Create Library object to represent the macro.
           |    lib = siliconcompiler.Library(chip, design)
           |    lib.set('output', stackup, 'gds', 'sram/${c.name}.gds')
           |    lib.set('output', stackup, 'lef', 'sram/${c.name}.lef')
           |    # Set the 'copy' field to True to pull these files into the build directory during
           |    # the 'import' task, which makes them available for the remote workflow to use.
           |    lib.set('output', stackup, 'gds', True, field='copy')
           |    lib.set('output', stackup, 'lef', True, field='copy')
           |
           |    return lib
           |""".stripMargin
      )
      sbf.close()

      val bbf = new BufferedWriter(new FileWriter(new File(dir, c.name + ".bb.v")))

      bbf.write(
        s"""// OpenRAM SRAM model
           |// Words: 1024
           |// Word size: 32
           |// Write size: 8
           |(* blackbox *)
           |module ${c.name}(
           |`ifdef USE_POWER_PINS
           |    vccd1,
           |    vssd1,
           |`endif
           |// Port 0: W
           |    clk0,csb0${if(c.bitPerMask.nonEmpty) ",wmask0" else ""},addr0,din0,
           |// Port 1: R
           |    clk1,csb1,addr1,dout1
           |  );
           |  ${if(c.bitPerMask.nonEmpty) s"parameter NUM_WMASKS = ${c.bitPerMask.get} ;" else ""}
           |  parameter DATA_WIDTH = ${c.dataWidth} ;
           |  parameter ADDR_WIDTH = ${log2Up(c.wordCount)} ;
           |  parameter RAM_DEPTH = 1 << ADDR_WIDTH;
           |  // FIXME: This delay is arbitrary.
           |  parameter DELAY = 3 ;
           |  parameter VERBOSE = 1 ; //Set to 0 to only display warnings
           |  parameter T_HOLD = 1 ; //Delay to hold dout value after posedge. Value is arbitrary
           |
           |`ifdef USE_POWER_PINS
           |    inout vccd1;
           |    inout vssd1;
           |`endif
           |  input  clk0; // clock
           |  input   csb0; // active low chip select
           |  input [ADDR_WIDTH-1:0]  addr0;
           |  ${if(c.bitPerMask.nonEmpty) s"input [NUM_WMASKS-1:0]  wmask0; // write mask" else ""}
           |  input [DATA_WIDTH-1:0]  din0;
           |  input  clk1; // clock
           |  input   csb1; // active low chip select
           |  input [ADDR_WIDTH-1:0]  addr1;
           |  output [DATA_WIDTH-1:0] dout1;
           |
           |endmodule
           |""".stripMargin
      )
      bbf.close()
    }

    println(macros.mkString(" \\\n"))

    {
      val f = new File("sram/openram.sh")
      val bf = new BufferedWriter(new FileWriter(f))
      bf.write(s"make ${macros.mkString(" ")}") // -j$$(nproc)
      bf.close()
    }

    {
      val f = new File("siliconcompiler.py")
      val bf = new BufferedWriter(new FileWriter(f))
      for(m <- macros) {
        bf.write(
          s"""    from sram import ${m}_sc
             |    chip.use(${m}_sc)
             |    chip.add('asic', 'macrolib', '$m')
             |    chip.input('sram/$m.bb.v')
             |""".stripMargin
        )
      }
      bf.close()
    }

  }
}