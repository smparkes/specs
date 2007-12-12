package org.specs.collection

trait ExtendedRange {
  var i = 0
  class ExtendedRange(range: Range) {
    def sum(exp: => Int) = range.foldRight(0) { (v: Int, result: Int) =>
      i = v
      result + exp
    }
  }
  implicit def foreach(r: Range) = {new ExtendedRange(r)}
}

