package scala.specs.matcher
import scala.specs._
import scala.specs.runner._
import scala.specs.Sugar._
import scala.specs.matcher.ScalacheckParameters._
import scalacheck._
import scalacheck.Gen._
import scala.specs.mock._
import scala.io._

object scalacheckMatchersTestSuite extends JUnit3(scalacheckMatchersSpec)
object scalacheckMatchersSpec extends MatchersSpecification with ScalacheckExamples {
  "A 'pass' matcher" should {
    "be ok if a property is true for all generated values" in {
      alwaysTrue must pass(isTrue)
    }
    "be ko if a property is false for a generated value" in {
      assertion(alwaysTrue must pass(isFalse)) must failWithMatch("A counter-example is 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if a assertion is false for a generated value. The failure message should be the assert ko message" in {
      assertion(random must pass(identityAssert)) must failWithMatch("A counter-example is 'false': 'false' is not the same as 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if checking the values generation yields an exception" in {
      assertion(exceptionValues must pass(isTrue)) must failWith("Exception \"java.lang.Exception\" raised on argument generation.")
    }
    "be ko if checking the property yields an exception during its evaluation" in {
      assertion(alwaysTrue must pass(exceptionProperty)) must failWith("Exception \"java.lang.Exception\" raised on property evaluation:\n> true")
    }
    "be ko if all values have been exhausted before the min number of ok tests is reached" in {
      assertion(Gen.fail[Boolean] must pass(isTrue)(set(maxDiscarded->10))) must failWith("Gave up after only 0 passed tests. 10 tests were discarded.")
    }
  }
}
trait ScalacheckExamples extends Specification {
  val alwaysTrue = elements(true)
  val alwaysFalse = elements(false)
  val random = elements(true, false)
  val exceptionValues = new Gen(p => throw new Exception)
  val isTrue = ((x: Boolean) => true)
  val isFalse = ((x: Boolean) => false)
  val identityAssert = ((x: Boolean) => x mustBe true)
  val exceptionProperty = ((x: Boolean) => {throw new Exception})
}
