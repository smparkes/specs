package org.specs.matcher
import org.specs._
import org.specs.runner._
import org.specs.Sugar._
import org.specs.matcher.ScalacheckParameters._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop.property
import org.specs.mock._
import org.specs.io._

class scalacheckMatchersTest extends JUnit3(scalacheckMatchersSpec)
object scalacheckMatchersSpecRunner extends ConsoleRunner(scalacheckMatchersSpec)
object scalacheckMatchersSpec extends MatchersSpecification with ScalacheckExamples {
  "A 'pass' matcher" should {
    "be ok if a property is true for all generated values" in {
      alwaysTrue must pass(isTrue)
    }
    "be ok with a true property" in {
      alwaysTrueProp must pass
    }
    "be ko with a false property" in {
      assertion(identityProp must pass) must failWithMatch("A counter-example is 'false' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if a property is false for a generated value" in {
      assertion(alwaysTrue must pass(isFalse)) must failWithMatch("A counter-example is 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if a assertion is false for a generated value. The failure message should be the assert ko message" in {
      assertion(random must pass(identityAssert)) must failWithMatch("A counter-example is 'false': 'false' is not the same as 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if checking the values generation yields an exception" in {
      assertion(exceptionValues must pass(isTrue)) must failWith("Exception \"java.lang.Exception: e\" raised on argument generation.")
    }
    "be ko if checking the property yields an exception during its evaluation" in {
      assertion(alwaysTrue must pass(exceptionProperty)) must failWith("Exception \"java.lang.Exception: e\" raised on property evaluation:\n> ARG_0 = true")
    }
    "be ko if all values have been exhausted before the min number of ok tests is reached" in {
      assertion(Gen.fail[Boolean] must pass(isTrue)(set(maxDiscarded->10))) must failWith("Gave up after only 0 passed tests. 10 tests were discarded.")
    }
    "accept properties based on scalacheck commands" in  {
      assertion(Counter.commandsProp must pass) must failWithMatch("A counter-example is .*")
    } 
  }
}
trait ScalacheckExamples extends Specification {
  val identityProp = property((a:Boolean) => a)
  val alwaysTrueProp = property((a:Int) => true)
  val alwaysTrue = elements(true)
  val alwaysFalse = elements(false)
  val random = elements(true, false)
  val exceptionValues = new Gen(p => throw new Exception("e"))
  val isTrue = ((x: Boolean) => true)
  val isFalse = ((x: Boolean) => false)
  val identityAssert = ((x: Boolean) => x mustBe true)
  val exceptionProperty = ((x: Boolean) => throw new Exception("e"))
}
object Counter extends org.scalacheck.Commands {

  type S = Int

  private var counter: Counter = null

  protected def initialState: Int = {
    counter = new Counter(0)
    counter.n
  }

  case class Inc extends Command {
    def apply(s: Int) = counter.inc
    def nextState(s: Int) = s+1
    override def postCondition(s: Int, result: Any) = counter.n == s+1
  }

  case class Dec extends Command {
    def apply(s: Int) = counter.dec
    def nextState(s: Int) = s-1
    override def postCondition(s: Int, result: Any) = counter.n == s-1
  }

  def genCommand(s: Int) = org.scalacheck.Gen.elements(Inc, Dec)
}

class Counter(var n: Int) {
  private var i = false;
  private var x = 0;

  def inc = {
    i = true
    x = 0
    n += 1
  }

  def dec = {
    if(i) x += 1
    if(x != 5) n -= 1
  }
}
