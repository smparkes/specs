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
      specWithOneExample(that.isOk) mustContain "A specification should"
    }
    "report the specification examples: '-have example 1 ok'" in { 
      specWithOneExample(that.isOk) mustContain "- have example 1 ok"
    }
    "display '0 failure' if there is no assertion" in { 
      specWithOneExample(that.isOk) must existMatch("0 failure")
    } 
    "display '1 failure' if one example isKo" in { 
      specWithOneExample(that.isKo) must existMatch("1 failure") 
    } 
    "display the first failure of an example having several ones" in { 
      specWithOneExample(that.isKo, that.isKo) must existMatch("first failure") 
      specWithOneExample(that.isKo, that.isKo) must notExistMatch("second failure")
    } 
    "display '1 error' if one example throws an exception" in { 
      specWithOneExample(that.throwsAnException) must existMatch("1 error") 
    } 
    "report a pluralized message if there are several examples failing" in { 
      specWithTwoExamples(that.isKo) must existMatch("2 examples")
      specWithTwoExamples(that.isKo) must existMatch("2 failures")
    } 
    "report the number of assertions: '2 assertions'" in { 
      specWithOneExample(that.isOk) must existMatch("1 assertion")
      specWithTwoExamples(that.isKo) must existMatch("2 assertions")
    } 
    "display the failure message next to the corresponding example" in { 
      specWithTwoExamples(that.isKo, that.isOk) verifies (messages =>
            messages.findIndexOf(matches("first failure")) ==
            messages.findIndexOf(matches("example 2.1 ok")) + 1)
    } 
    "report the elapsed time" in { 
      specWithOneExample(that.isOk) mustExistMatch "Finished in"
    }
    "have a 'fail' method adding a new failure to the last example" in {
      specWithOneExample(that.isKoWithTheFailMethod) mustExistMatch "1 failure" 
    }
  }

  def specWithOneExample(assertions: (that.Value)*) = new SpecWithOneExample(assertions.toList).run
  def specWithTwoExamples(assertions: (that.Value)*) = new SpecWithTwoExamples(assertions.toList).run
}

abstract class TestSpec extends Specification with ConsoleReporter with MockOutput {
  val success = () => true mustBe true
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val failMethod = () => fail("failure with the fail method")
  val exception = () => throw new Error("new Error")
  def assertions(behaviours: List[that.Value]) = behaviours map { case that.isOk => success
                                    case that.isKo => failure1
                                    case that.isKoTwice => () => {failure1(); failure2()} 
                                    case that.isKoWithTheFailMethod => failMethod 
                                    case that.throwsAnException => exception }
}

class SpecWithOneExample(behaviours: List[(that.Value)]) extends TestSpec {
  def run = {
    "A specification" should {
      "have example 1 ok" in {
        assertions(behaviours) foreach {_.apply}
      }
    }
    report(this.suts)
    messages
  }   
}

class SpecWithTwoExamples(behaviours: List[(that.Value)]) extends TestSpec {
  def run = {
    "A specification" should {
      "have example 2.1 ok" in { assertions(behaviours).head.apply}
      "have example 2.2 ok" in { assertions(behaviours).last.apply }
    }
    report(this.suts)
    messages
  }   
}
object that extends Enumeration {
  val isKo, isOk, isKoTwice, isKoWithTheFailMethod, throwsAnException = Value
}

