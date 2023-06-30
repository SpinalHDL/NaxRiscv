// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package naxriscv.utilities

import java.io.{File, PrintWriter}
import scala.collection.mutable

class DocPlugin extends Plugin {
  val properties = mutable.LinkedHashMap[String, Any]();
  def property(key : String, value : Any) = properties(key) = value

  def genC(path : String = "nax.h"): Unit ={
    val writer = new PrintWriter(path)

    writer.println("#pragma once\n")
    for(p <- properties){
      writer.println(s"#define ${p._1.toUpperCase} ${p._2}")
    }
    writer.flush()
    writer.close()
  }
}
