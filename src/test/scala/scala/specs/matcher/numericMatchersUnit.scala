package scala.specs.matcher
import scala.specs._
import scala.specs.runner._
import scalacheck.Gen._

object numericMatchersSuite extends JUnit3(numericMatchersUnit)
object numericMatchersUnit extends MatchersSpecification {
  "A 'beClose' matcher" should {
    "be ok if x is inside the range n - delta, n + delta" in {
      case class RangeMatch(x: Double, n: Double, delta: Double)
      val cases  = for (n <- choose(-5, 5);
                        delta <- choose(0, 3);
                        x <- choose(n - delta, n + delta))
                        yield RangeMatch(x, n, delta)
      cases must pass { r: RangeMatch => val RangeMatch(x, n, delta) = r
        x must beCloseTo(n, delta)
      }
    }
  }
}
