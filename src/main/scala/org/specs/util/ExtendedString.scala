package org.specs.util

/** The ExtendedString object adds utility functions like 'uncapitalize' to Strings */
object ExtendedString {
  /** @return an ExtendedString */
  implicit def toExtendedString(s: String) = ExtendedString(s) 

  /** This class adds utility functions to Strings */
  case class ExtendedString(s: String) {
    /** @return the String s with its first character being uncapitalized: "HELLO".uncapitalize -> "hELLO" */
    def uncapitalize = s.first.toLowerCase + s.drop(1)

    /** 
     * @param remove String to suppress from the original string
     * @return a String where every occurrence of remove has been suppressed 
     */
    def removeAll(remove: String) = s.replaceAll(toReplace(remove), "")
    private def toReplace(c: String) = c.map { letter => if ("()[]{}+-\\^$|?.*".contains(letter)) ("\\" + letter) else letter }.mkString("")
  }
}
