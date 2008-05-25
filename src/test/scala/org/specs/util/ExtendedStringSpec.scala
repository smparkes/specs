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

}
