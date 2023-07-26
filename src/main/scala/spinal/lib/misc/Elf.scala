package spinal.lib.misc

import net.fornwall.jelf.{ElfFile, ElfSection, ElfSectionHeader}
import spinal.lib.sim.SparseMemory

import java.io.File
import java.nio.file.Files

class Elf(val f : File){
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
    println(f"${section.header.getName} ${memoryAddress}%x ${size}")
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
        val memoryAddress = section.header.sh_addr + offset
        mem.write(memoryAddress, data)
      }
    }
  }

  def getSymbolAddress(name : String): Long ={
    val s = elf.getELFSymbol(name)
    s.st_value
  }
}

object ElfTest extends App{
  import net.fornwall.jelf._
  val elf = new Elf(new File("ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf"))

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