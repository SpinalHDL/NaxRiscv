// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

import spinal.core.BaseType
import spinal.lib.pipeline.Stageable

package object naxriscv {
  def DecodeList(e : (Stageable[_ <: BaseType],Any)*) = List(e :_*)
  type DecodeListType = Seq[(Stageable[_ <: BaseType],Any)]
}
