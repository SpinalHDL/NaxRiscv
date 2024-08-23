package spinal.lib.misc

import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader, ElfSymbol, ElfSymbolTableSection}
import spinal.lib.sim.SparseMemory
import spinal.core._

import java.io.File
import java.nio.file.Files

class Elf(val f : File, addressWidth : Int){
  val fBytes = Files.readAllBytes(f.toPath)
  val elf = ElfFile.from(fBytes)

  def foreachSection(body : ElfSection => Unit): Unit ={
    for(sectionId <- 0 until elf.e_shnum) {
      val section = elf.getSection(sectionId)
      body(section)
    }
  }

  def getData(section : ElfSection): Array[Byte] ={
    val fileAddress = section.header.sh_offset
    val memoryAddress = section.header.sh_addr
    val size = section.header.sh_size.toInt
    // println(f"${section.header.getName} ${memoryAddress}%x ${size}")
    if(size == 0) return Array.fill(0)(0.toByte)

    val ret = new Array[Byte](size)
    if(section.header.sh_type != ElfSectionHeader.SHT_NOBITS && section.header.sh_type != ElfSectionHeader.SHT_NULL) {
      Array.copy(fBytes, fileAddress.toInt, ret, 0, size)
    }
    ret
  }

  def load(mem : SparseMemory, offset : Long): Unit ={
    foreachSection{section =>
      if((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0){
        val data = getData(section)
        val memoryAddress = (section.header.sh_addr - offset) & ((BigInt(1) << addressWidth)-1).toLong
        mem.write(memoryAddress, data)
      }
    }
  }


  def getMemInit[T <: Data](ram: Mem[T],offset: BigInt, allowOverflow: Boolean = false) = {
    val wordSize = ram.wordType.getBitsWidth / 8
    val initContent = Array.fill[BigInt](ram.wordCount)(0)
    foreachSection { section =>
      if ((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0) {
        val data = getData(section)
        val memoryAddress = (section.header.sh_addr - offset) & ((BigInt(1) << addressWidth) - 1).toLong
        for((byte, i) <- data.zipWithIndex){
          val addressWithoutOffset = memoryAddress+i
          val addressWord = addressWithoutOffset / wordSize
          if (addressWord < 0 || addressWord >= initContent.size) {
            assert(allowOverflow)
          } else {
            initContent(addressWord.toInt) |= BigInt(byte.toInt & 0xFF) << ((addressWithoutOffset.toInt % wordSize) * 8)
          }
        }
      }
    }
    initContent
  }

  def init[T <: Data](ram: Mem[T], offset: BigInt, allowOverflow: Boolean = false): Unit = {
    val initContent = getMemInit(ram, offset, allowOverflow)
    ram.initBigInt(initContent)
  }

  def load[T <: Data](ram: Mem[T], offset: BigInt, allowOverflow: Boolean = false): Unit = {
    val initContent = getMemInit(ram, offset, allowOverflow)
    import spinal.core.sim._
    for((e, i) <- initContent.zipWithIndex){
      ram.setBigInt(i, e)
    }
  }

  def getSymbolAddress(name : String): Long ={
    val s = getELFSymbol(name)
    s.st_value
  }

  def getELFSymbol(symbolName: String): ElfSymbol = {
    if (symbolName == null) return null
    // Check dynamic symbol table for symbol name.
    import elf._
    var sh = getDynamicSymbolTableSection
    if (sh != null) {
      val numSymbols = sh.symbols.length
      var i = 0
      while ( {
        i < numSymbols
      }) {
        var symbol = sh.symbols(i)
        if (symbolName == symbol.getName) return symbol
        else if (symbolName == (sh.symbols(numSymbols - 1 - i)).getName) return sh.symbols(numSymbols - 1 - i)

        i += 1
      }
    }
    // Check symbol table for symbol name.
    sh = getSymbolTableSection
    if (sh != null) {
      val numSymbols = sh.symbols.length
      var i = 0
      while ( {
        i < numSymbols
      }) {
        var symbol = sh.symbols(i)
        if (symbolName == symbol.getName) return symbol
        else if (symbolName == (sh.symbols(numSymbols - 1 - i)).getName) return sh.symbols(numSymbols - 1 - i)

        i += 1
      }
    }
    null
  }
}

object ElfTest extends App{
  import net.fornwall.jelf._
  val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"), 32)

  elf.foreachSection{section =>
    println(f"${section.header.getName} ${section.header.sh_type} ${section.header.sh_flags}")
    if((section.header.sh_flags & ElfSectionHeader.FLAG_ALLOC) != 0){
      val data = elf.getData(section)
      println(section.header.getName)
      println(data)
    }
  }
  val a = elf.getSymbolAddress("_start")
  println(a)
}