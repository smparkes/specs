package org.specs.runner

import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.util._
import scala.collection.mutable._
import org.specs.io.mock.MockOutput
import org.specs.Sugar._
import org.specs.matcher.MatcherUtils._
import org.specs.util.ExtendedString._

object consoleReporterSpec extends Specification with MockOutput {
  "A console reporter" should {
    "report the name of the specification: 'A specification should'" in {
      specWithOneExample(that.isOk) must containMatch("A specification should")
    }
    "report the specification examples: '-have example 1 ok'" in { 
      specWithOneExample(that.isOk) must containMatch("have example 1 ok")
    }
    "display '0 failure' if there is no assertion" in { 
      specWithOneExample(that.isOk) must existMatch("0 failure")
    } 
    "display '1 failure' if one example isKo" in { 
      specWithOneExample(that.isKo) must existMatch("1 failure") 
    } 
    "indicate the line and class where the failure occurred" in { 
      specWithOneExample(that.isKo) must existMatch("(consoleReporterSpec.scala:\\d)") 
    } 
    "display the first failure of an example having several ones" in { 
      specWithOneExample(that.isKo, that.isKo) must existMatch("first failure") 
      specWithOneExample(that.isKo, that.isKo) must notExistMatch("second failure")
    } 
    "display '1 error' if one example throws an exception" in {
      specWithOneExample(that.throwsAnException) must existMatch("1 error") 
    } 
    "display '1 skipped' if one example is skipped" in { 
      specWithOneExample(that.isSkipped) must existMatch("1 skipped") 
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
      specWithTwoExamples(that.isKo, that.isOk) verifies(messages =>
            messages.findIndexOf(matches("first failure")) ==
            messages.findIndexOf(matches("example 2.1 ok")) + 1)
    } 
    "report the elapsed time" in { 
      specWithOneExample(that.isOk) mustExistMatch "Finished in"
    }
    "report failures created with the 'fail' method" in {
      specWithOneExample(that.isKoWithTheFailMethod) mustExistMatch "1 failure" 
    }
    "report skipped examples created with the 'skip' method with a small circle" in {
      specWithOneExample(that.isSkipped) mustExistMatch "o " 
    }
    "report skipped examples created with the 'orSkipExample' on a faulty matcher with a small circle" in {
      specWithOneExample(that.isSkippedBecauseOfAFaultyMatcher) mustExistMatch "o " 
    }
    "report the literal description of a sus if it is set"  in {
      new SpecWithLiterateDescription(that.isOk).run mustExistMatch "Some text with embedded assertions"
    }
    "report the reason for a skipped example" in {
      specWithOneExample(that.isSkipped) mustExistMatch "irrelevant" 
    }
    "indicate the line and class where the skipping occurred" in { 
      specWithOneExample(that.isSkipped) must existMatch("(consoleReporterSpec.scala:\\d)") 
    } 
    "report the time for each system and add times for the total" in {
      specWithTwoSystems.messages
      val susTime1 :: susTime2 :: total :: Nil = specWithTwoSystems.elapsedTimes
      (susTime1 + susTime2) must beCloseTo(total, 1) // to account for rounding errors
    }
  }
  "A console reporter" should {
    "not print stack trace if setNoStackTrace is called" in {
      val spec = new SpecWithOneExample(that.throwsAnException)
      spec.setNoStacktrace
      spec.run mustNot containMatch("org.specs.runner.SpecWithOneExample\\$")
    }
  }
  "A console trait" should {
    "setNoStackTrace on the ConsoleReporter when passed the -ns or --nostacktrace argument" in {
      val testSpecRunner = new SpecWithOneExample(that.throwsAnException) with MockOutput 
      testSpecRunner.args ++= Array("-ns")
      testSpecRunner.reportSpecs
      testSpecRunner.messages mustNot containMatch("org.specs.runner.SpecWithOneExample\\$")
    }
  }
  "A console trait" can { clean.before
    "accept a --reject argument to only exclude examples having some tags in the specification" in {
      runWith("--reject", "out") must (containMatch("\\+ included") and containMatch("o excluded")) 
    }
    "accept a -rej argument to only exclude examples having some tags in the specification" in {
      runWith("-rej", "out") must (containMatch("\\+ included") and containMatch("o excluded")) 
    }
    "accept a --accept argument to only include examples having some tags in the specification" in {
      runWith("--accept", "in") must (containMatch("\\+ included") and containMatch("o excluded")) 
    }
    "accept a -acc argument to only exclude examples having some tags in the specification" in {
      runWith("-acc", "in") must (containMatch("\\+ included") and containMatch("o excluded")) 
    }
  }
  "A console trait" should { clean.before
    "print a warning message if a accept/reject argument is not followed by tags" in {
      runWith("-acc") must containMatch("\\[WARNING\\] accept/reject tags omitted") 
    }
    "work with several tags separated by a comma" in {
      runWith("-acc", "in,out") must (containMatch("\\+ included") and containMatch("\\+ excluded"))
    }
  } 
  def runWith(args: String*): List[String] = {
    specRunner.args = args.toArray
    specRunner.reportSpecs
    specRunner.messages.toList
  }
  def clean = {
    specRunner.args = Array()
    spec.acceptAnyTag
    spec.resetForExecution
    specRunner.messages.clear
  }
  object spec extends Specification { 
    ("excluded" in {}).tag("out") 
    ("included" in {}).tag("in") 
  }
  object specRunner extends ConsoleRunner(spec) with MockOutput

  def specWithOneExample(assertions: (that.Value)*) = new SpecWithOneExample(assertions.toList).run
  def specWithTwoExamples(assertions: (that.Value)*) = new SpecWithTwoExamples(assertions.toList).run
  def specWithTwoSystems = new SpecWithTwoSystems().run
}
abstract class TestSpec extends LiterateSpecification with Console with MockOutput {
  val specs = List(this)
  override def main(args: Array[String]) = super[Console].main(args)
  val success = () => true mustBe true
  val isSkipped = () => skip("irrelevant")
  val isSkippedBecauseOfAFaultyMatcher = () => 1 must be(0).orSkipExample
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val failMethod = () => fail("failure with the fail method")
  val exception= () => throw new Exception("new Error")
  def assertions(behaviours: List[that.Value]) = behaviours map { 
                                    case that.isOk => success
                                    case that.isSkipped => isSkipped
                                    case that.isSkippedBecauseOfAFaultyMatcher => isSkippedBecauseOfAFaultyMatcher
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
    reportSpecs
    messages
  }   
}

class SpecWithTwoExamples(behaviours: List[(that.Value)]) extends TestSpec {
  def run = {
    "A specification" should {
      "have example 2.1 ok" in { assertions(behaviours).head.apply}
      "have example 2.2 ok" in { assertions(behaviours).last.apply }
    }
    reportSpecs
    messages
  }   
}
class SpecWithTwoSystems extends TestSpec {
  def elapsedTimes = messages.flatMap(_.groups("Finished in .* (\\d+) ms")).filter(!_.isEmpty).toList.map(_.toInt)
  def run = {
    messages.clear
    "A specification" should {
      "have example 2.1 ok" in { Thread.sleep(10) }
      "have example 2.2 ok" in { Thread.sleep(10) }
    }
    "A specification" should {
      "have example 2.1 ok" in { Thread.sleep(10) }
      "have example 2.2 ok" in { Thread.sleep(10) }
    }
    reportSpecs
    messages
    this
  }   
}
class SpecWithLiterateDescription(behaviours: List[(that.Value)]) extends TestSpec {
  def run = {
    "The specification" is <p> 
      Some text with {"embedded assertions" in {assertions(behaviours) foreach {_.apply}}}
    </p>
    reportSpecs
    messages
  }   
}

object that extends Enumeration {
  val isKo, isOk, isKoTwice, isKoWithTheFailMethod, throwsAnException, isSkipped, isSkippedBecauseOfAFaultyMatcher = Value
}
