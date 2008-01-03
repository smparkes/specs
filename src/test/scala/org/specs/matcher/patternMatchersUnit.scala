package org.specs.matcher
import org.specs._
import org.specs.runner._
import org.specs.Sugar._

class patternMatchersTest extends JUnit3(patternMatchersUnit)
object patternMatchersUnit extends MatchersSpecification {
  "A 'beLike' pattern matcher" should {
    "be ok even with a null pattern" in {
      val pattern : (Any => Boolean) = null
      assertion("name" must beLike(pattern)) must failWith("'name' doesn't match the expected pattern")
    }
    "be ok even with a null value" in {
      val name : String = null
      assertion(name must beLike {case s: String => ok}) must failWith("'null' doesn't match the expected pattern")
    }
  }
  "A 'beSome' pattern matcher" should {
    "be ok even with a null value" in {
      val value : Option[String] = null
      assertion(value must beSome[String]) must failWith("'null' is not Some(x)")
    }
    "be ok even with a null pattern for the which function" in {
      val pattern : (Any => Boolean) = null
      assertion(Some("name") must beSome[String].which(pattern)) must failWith("the 'which' property is a null function")
    }
  }
}
