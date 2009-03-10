package org.specs.matcher
import org.specs._
import scalacheck.Gen._

class numericMatchersUnit extends MatchersSpecification with ScalaCheck {
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
  "Numeric matchers" should {
    "not evaluate the expressions twice: be_>" in {
      be_>(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_>=" in {
      be_>=(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_<" in {
      be_<(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_<=" in {
      be_<=(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_closeTo" in {
      beCloseTo(1, 0) must evalOnce(exp(1))
    }
  }
}
