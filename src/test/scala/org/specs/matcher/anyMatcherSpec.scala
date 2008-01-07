package org.specs.matcher
import org.specs.runner._
import org.specs.specification._

class anyMatcherSpecTest extends JUnit3(anyMatcherSpec)
object anyMatcherRunner extends ConsoleRunner(anyMatcherSpec)
object anyMatcherSpec extends MatchersSpecification {
  "A matcher" can {
    "be created as a case class" in {
      case class matchHello(a: String) extends Matcher[String]() {
        def apply(v: => String) = (v == a, "okMessage", "koMessage")
      }
      "hello" must matchHello("hello")
    }
    "be created as a val" in {
      val beEven = new Matcher[Int] {
        def apply(b: => Int) = (b % 2 == 0, b + " is even", b + " is odd")
      }
      2 must beEven
    }
    "be created as a method" in {
      def divide(a: Int) = new Matcher[Int] {
        def apply(b: => Int) = (a % b == 0, b + " divides " + a, b + " doesn't divide " + a)
      }
      10 must divide(100)
      3 must not(divide(100))
    }
    "be skipped" in {
      assertion( 1 must be(2).orSkipExample ) must throwA(SkippedException("'1' is not '2'"))
    }
  }

}
