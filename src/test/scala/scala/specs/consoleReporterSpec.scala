package scala.specs

import scala.specs._
import scala.specs.integration._
import scala.util._
import scala.collection.mutable._
import scala.io.mock.MockOutput
import junit.framework._

object consoleReporterSuite extends JUnit3TestSuite(consoleReporterSpec)
object consoleReporterSpec extends Specification with Sugar {

  "A console reporter" should {
    "report the name of the specification: 'A specification should'" in { 
      specWithOneExample(that.succeeds) mustContain "A specification should"
    }
    "report the specification examples: '-have example 1 ok'" in { 
      specWithOneExample(that.succeeds) mustContain "- have example 1 ok"
    }
    "display '0 failure' if there is no assertion" in { 
      specWithOneExample(that.succeeds) must existMatch("0 failure")
    } 
    "display '1 failure' if one example fails" in { 
      specWithOneExample(that.fails) must existMatch("1 failure") 
    } 
    "display the first failure of an example having several ones" in { 
      specWithOneExample(that.fails, that.fails) must existMatch("first failure") 
      specWithOneExample(that.fails, that.fails) must notExistMatch("second failure")
    } 
    "display '1 error' if one example throws an exception" in { 
      specWithOneExample(that.throwsAnException) must existMatch("1 error") 
    } 
    "report a pluralized message if there are several examples failing" in { 
      specWithTwoExamples(that.fails) must existMatch("2 examples")
      specWithTwoExamples(that.fails) must existMatch("2 failures")
    } 
    "report the number of assertions: '2 assertions'" in { 
      specWithOneExample(that.succeeds) must existMatch("1 assertion")
      specWithTwoExamples(that.fails) must existMatch("2 assertions")
    } 
    "display the failure message next to the corresponding example" in { 
      specWithTwoExamples(that.fails, that.succeeds) verifies (messages =>
            messages.findIndexOf(matches("first failure")) ==
            messages.findIndexOf(matches("example 2.1 ok")) + 1)
    } 
    "report the elapsed time" in { 
      specWithOneExample(that.succeeds) mustExistMatch "Finished in"
    }
    "have a 'fail' method adding a new failure to the last example" in {
      specWithOneExample(that.failsWithTheFailMethod) mustExistMatch "1 failure" 
    }
  }

  def specWithOneExample(assertions: (that.Value)*) = new SpecWithOneExample(assertions.toList).run
  def specWithTwoExamples(assertions: (that.Value)*) = new SpecWithTwoExamples(assertions.toList).run
}

abstract class TestSpec(behaviours: List[that.Value]) extends Specification with ConsoleReporter with MockOutput {
  val ok = () => true mustBe true
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val failMethod = () => fail("failure with the fail method")
  val exception = () => throw new Error("new Error")
  def assertions = behaviours map { case that.succeeds => ok
                                    case that.fails => failure1
                                    case that.failsTwice => failure2 
                                    case that.failsWithTheFailMethod => failMethod 
                                    case that.throwsAnException => exception }
}

class SpecWithOneExample(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
  def run = {
    "A specification" should {
      "have example 1 ok" in {
        assertions foreach {_.apply}
      }
    }
    report(this.suts)
    messages
  }   
}

class SpecWithTwoExamples(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
  def run = {
    "A specification" should {
      "have example 2.1 ok" in { assertions.head.apply}
      "have example 2.2 ok" in { assertions.last.apply }
    }
    report(this.suts)
    messages
  }   
}
object that extends Enumeration {
  val fails, succeeds, failsTwice, failsWithTheFailMethod, throwsAnException = Value
}

