package org.quasigroup.fix

class StandardTrailer {
  def toFIX(sum: Int): String = {

    Field(Tag(10), Value(f"${sum % 256}%03d")).output
  }
}
