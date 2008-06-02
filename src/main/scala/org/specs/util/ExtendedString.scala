package org.specs.util

object ExtendedString {
  implicit def toExtendedString(s: String) = ExtendedString(s) 
  case class ExtendedString(s: String) {
    def uncapitalize = s.first.toLowerCase + s.drop(1)
    def removeAll(c: String) = s.replaceAll(toReplace(c), "")
    private def toReplace(c: String) = c.map { letter => if ("()[]{}+-\\^$|?.*".contains(letter)) ("\\" + letter) else letter }.toString
  }
}
