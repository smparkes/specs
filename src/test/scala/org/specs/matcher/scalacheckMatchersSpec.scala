package org.specs.matcher
import org.specs._
import org.specs.runner._
import org.specs.Sugar._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop.property
import org.specs.mock._
import org.specs.io._

class scalacheckMatchersTest extends Runner(scalacheckMatchersSpec) with JUnit
object scalacheckMatchersSpec extends MatchersSpecification with ScalacheckExamples {
  "A 'pass' matcher" should {
    "be ok if a property is true for all generated values" in {
      alwaysTrue must pass(isTrue)
    }
    "be ok with a true property" in {
      alwaysTrueProp must pass
    }
    "be ko with a false property" in {
      assertion(identityProp must pass) must failWithMatch("A counter-example is 'false' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a property is false for a generated value" in {
      assertion(alwaysTrue must pass(isFalse)) must failWithMatch("A counter-example is 'true' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a assertion is false for a generated value. The failure message should be the assert ko message" in {
      assertion(random must pass(identityAssert)) must failWithMatch("A counter-example is 'false': 'false' is not the same as 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if checking the values generation yields an exception" in {
      assertion(exceptionValues must pass(isTrue)) must failWithMatch("Exception raised on argument generation")
    }
    "be ko if checking the property yields an exception during its evaluation" in {
      assertion(alwaysTrue must pass(exceptionProperty)) must failWithMatch("Exception raised on property evaluation")
    }
    "be ko if all values have been exhausted before the min number of ok tests is reached" in {
      assertion(Gen.fail[Boolean] must pass(isTrue)(set(maxDiscarded->10))) must failWith("Gave up after only 0 passed tests. 10 tests were discarded.")
    }
    "accept properties based on scalacheck commands" in  {
      assertion(CounterSpecification must pass) must failWithMatch("A counter-example is .*")
    } 
  }
  "A ScalaCheck property" should {
    "add new assertions during evaluation if isAssertion is on" in {
      spec.assertionsNb must be_==(101)
    }
    "add new assertions during evaluation if assertProperties is on (default)" in {
      specWithAssertProperties.assertionsNb must be_==(101)
    }
    "not add new assertions during evaluation if dontAssertProperties is on (default)" in {
      specWithDontAssertProperties.assertionsNb must be_==(1)
    }
    "count a new assertion for each time the property is evaluated + one for the pass assertion" in {
      specWithFailure.assertionsNb must be_==(11)
    }
  }
}
object spec extends Specification with Scalacheck {
  dontAssertProperties()
  property((a:Int) => isAssertion(a == a)) must pass
}
object specWithAssertProperties extends Specification with Scalacheck {
  property((a:Int) => a == a) must pass
}
object specWithDontAssertProperties extends Specification with Scalacheck {
  dontAssertProperties()
  property((a:Int) => a == a) must pass
}
object specWithFailure extends Specification with Scalacheck {  assertProperties
  var counter = 0
  property((a:Int) => {counter +=1; counter < 10}) must pass
}

trait ScalacheckExamples extends Specification with Scalacheck {
  val identityProp = property((a:Boolean) => a)
  val alwaysTrueProp = property((a:Int) => true)
  val alwaysTrue = elements(true)
  val alwaysFalse = elements(false)
  val random = elements(true, false)
  val exceptionValues = Gen(p => throw new Exception("e"))
  val isTrue = ((x: Boolean) => true)
  val isFalse = ((x: Boolean) => false)
  val identityAssert: Boolean => Boolean = ((x: Boolean) => x mustBe true)
  val exceptionProperty = ((x: Boolean) => throw new Exception("e"))
}
object CounterSpecification extends Commands {

  val counter = new Counter(0)

  case class State(n: Int)

  def initialState() = {
    counter.reset
    State(counter.get)
  }

  case object Inc extends Command {
    def run(s: State) = counter.inc
    def nextState(s: State) = State(s.n + 1)

    preCondition = s => true

    postCondition = (s,r) => counter.get == s.n + 1
  }

  case object Dec extends Command {
    def run(s: State) = counter.dec
    def nextState(s: State) = State(s.n - 1)
    postCondition = (s,r) => counter.get == s.n - 1
  }


  def genCommand(s: State): Gen[Command] = Gen.elements(Inc, Dec)

}
class Counter(private var n: Int) {
  def inc = n += 1 
  def dec = if (n > 3) n else n -= 1
  def get = n
  def reset = n = 0
}
