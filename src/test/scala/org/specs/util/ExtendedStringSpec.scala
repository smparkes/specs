package org.specs.util
import org.specs._
import org.specs.runner._
import org.specs.util.ExtendedString._

class ExtendedStringTest extends JUnit4(ExtendedStringSpec)
object ExtendedStringSpec extends Specification {
  "the uncapitalize function" should {
    "lower-case only the first letter of a string" in {
      "Hello".uncapitalize must_== "hello"     
    }
    "lower-case only the first letter of a one letter string" in {
      "H".uncapitalize must_== "h"     
    }
  }
  "the removeAll function" should {
    "remove a simple character" in {
      "hello".removeAll("l") must_== "heo"
    }
    "remove two characters" in {
      "hello".removeAll("lo") must_== "hel"
    }
    "remove regexp characters" in {
      "he(l)(l)o".removeAll(")(") must_== "he(ll)o"
    }
  }
}
