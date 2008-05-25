package org.specs.util

object ExtendedString {
  implicit def toExtendedString(s: String) = ExtendedString(s) 
  case class ExtendedString(s: String) {
    def uncapitalize = s.first.toLowerCase + s.drop(1)
  }
}
