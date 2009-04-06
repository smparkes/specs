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
import org.specs.execute._

class consoleReporterSpec extends Specification with JUnit {
  "A console reporter" should {
    "report the name of the specification: 'A specification should'" in {
      specWithOneExample(that.isOk) must containMatch("A specification should")
    }
    "report the specification examples: '-have example 1 ok'" in {
      specWithOneExample(that.isOk) must containMatch("have example 1 ok")
    }
    "display '0 failure' if there is no expectation" in {
      specWithOneExample(that.isOk) must containMatch("0 failure")
    }
    "display '1 failure' if one example isKo" in {
      specWithOneExample(that.isKo) must containMatch("1 failure")
    }
    "indicate the line and class where the failure occurred" in {
      specWithOneExample(that.isKo) must containMatch("(consoleReporterSpec.scala:\\d)")
    }
    "display the first failure of an example having several failures" in {
      specWithOneExample(that.isKo, that.isKo) must containMatch("first failure")
      specWithOneExample(that.isKo, that.isKo) must notContainMatch("second failure")
    }
    "display the failures of subexamples" in {
      specWithOneExample(that.hasTwoSubExamples) must containMatchOnlyOnce("sub1 failed")
    }
    "display '1 error' if one example throws an exception" in {
      specWithOneExample(that.throwsAnException) must containMatch("1 error")
    }
    "display the exception type if one example throws an exception" in {
      specWithOneExample(that.throwsAnException) must containMatch("java.lang.Exception")
    }
    "display '1 skipped' if one example is skipped" in {
      specWithOneExample(that.isSkipped) must containMatch("1 skipped")
    }
    "report a pluralized message if there are several examples failing" in {
      specWithTwoExamples(that.isKo) must containMatch("2 examples")
      specWithTwoExamples(that.isKo) must containMatch("2 failures")
    }
    "report the number of expectations: '2 expectations'" in {
      specWithOneExample(that.isOk) must containMatch("1 expectation")
      specWithTwoExamples(that.isKo) must containMatch("2 expectations")
    }
    "display the failure message next to the corresponding example" in {
      specWithTwoExamples(that.isKo, that.isOk) verifies(messages =>
            messages.findIndexOf(matches("first failure")) ==
            messages.findIndexOf(matches("example 2.1 ok")) + 1)
    }
    "report the elapsed time" in {
      specWithOneExample(that.isOk) mustContainMatch "Finished in"
    }
    "report failures created with the 'fail' method" in {
      specWithOneExample(that.isKoWithTheFailMethod) mustContainMatch "1 failure"
    }
    "report skipped examples created with the 'skip' method with a small circle" in {
      specWithOneExample(that.isSkipped) mustContainMatch "o "
    }
    "report skipped examples created with the 'orSkipExample' on a faulty matcher with a small circle" in {
      specWithOneExample(that.isSkippedBecauseOfAFaultyMatcher) mustContainMatch "o "
    }
    "report the literal description of a sus if it is set"  in {
      new SpecWithLiterateDescription(that.isOk).run mustContainMatch "Some text with embedded expectations"
    }
    "report the reason for a skipped example" in {
      specWithOneExample(that.isSkipped) mustContainMatch "irrelevant"
    }
    "indicate the line and class where the skipping occurred" in {
      specWithOneExample(that.isSkipped) must containMatch("(consoleReporterSpec.scala:\\d)")
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
      spec.setNoStacktrace()
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
    "accept a -xOnly (--failedOnly) argument to only show failed and error examples" in {
      runWith("-xOnly") must (notContainMatch("\\+ included") and containMatch("x failed") and containMatch("x error"))
      runWith("--failedOnly") must (notContainMatch("\\+ included") and containMatch("x failed") and containMatch("x error"))
    }
    "not display the sus at all if all examples are ok with the -xOnly flag" in {
      runWith("-acc", "in", "-xOnly") must notContainMatch("this sus")
    }
    "not display the statistics with the -finalstats or --finalstatistics flag" in {
      run2SystemsWith("-finalstats") must notContainMatch("for SUS")
    }
    "not display the statistics with the -nostats or --nostatistics flag" in {
      runWith("-nostats") must notContainMatch("Total time")
    }
    "print a help message with the options description if passed the -h or --help flag" in {
      mainWith("--help") must containMatch("--help")
    }
    "not execute the specification when passed the -h or --help flag" in {
      mainWith("--help") must notContainMatch("this sus")
    }
    def asString(s: String) = s.replace("\\", "\\\\").replace("[", "\\[")
    "report statuses with ANSI color codes when passed the -c or --color flag" in {
      runWith("--color") must containMatch(asString(AnsiColors.green))
    }
    "report a success in green when passed the -c or --color flag" in {
      runWith("-c", "-ex", "included") must containMatch(asString(AnsiColors.green))
    }
    "report a failure in red when passed the -c or --color flag" in {
      runWith("-c", "-ex", "failure") must containMatch(asString(AnsiColors.red))
    }
    "report an error in red when passed the -c or --color flag" in {
      runWith("-c", "-ex", "error") must containMatch(asString(AnsiColors.red))
    }
    "report a skipped example in yellow when passed the -c or --color flag" in {
      runWith("-c", "-ex", "skipped") must containMatch(asString(AnsiColors.yellow))
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
  def run2SystemsWith(args: String*): List[String] = {
    specTwoSystemsRunner.args = args.toArray
    specTwoSystemsRunner.reportSpecs
    specTwoSystemsRunner.messages.toList
  }
  def mainWith(args: String*): List[String] = {
    specRunner.main(args.toArray)
    specRunner.messages.toList
  }
  def clean = {
    specTwoSystemsRunner.resetOptions()
    specRunner.resetOptions
    spec.acceptAnyTag
    spec.resetForExecution
    specTwoSystems.acceptAnyTag
    specTwoSystems.resetForExecution
    specRunner.messages.clear
  }
  object spec extends Specification {
    "this sus" should {
      ("excluded" in {}).tag("out")
      ("included" in {}).tag("in")
      "failed" in { throw new FailureException("failed") }
      "error" in { throw new Error("error") }
      "skipped" in { skip("skipped") }
    }
  }
  object specRunner extends ConsoleRunner(spec) with MockOutput
  object specTwoSystems extends Specification {
    "this is system one" should { "do nothing" in {} }
    "this is system two" should { "do nothing" in {} }
  }
  object specTwoSystemsRunner extends ConsoleRunner(specTwoSystems) with MockOutput
  def specWithTwoSystems = new SpecWithTwoSystems().run
  def specWithOneExample(expectations: (that.Value)*) = new SpecWithOneExample(expectations.toList).run
  def specWithTwoExamples(expectations: (that.Value)*) = new SpecWithTwoExamples(expectations.toList).run
}
abstract class TestSpecification extends LiterateSpecification with Console with MockOutput {
  override val specs = List(this)
  override def main(args: Array[String]) = super[Console].main(args)
  val success = () => true mustBe true
  val isSkipped = () => skip("irrelevant")
  val isSkippedBecauseOfAFaultyMatcher = () => 1 must be(0).orSkipExample
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val failMethod = () => fail("failure with the fail method")
  val exception= () => throw new Exception("new Error")
  val subExamples = () => { "subexample1" in fail("sub1 failed"); "subexample2" in fail("sub2 failed") }
  def expectations(behaviours: List[that.Value]) = behaviours map {
                                    case that.isOk => success
                                    case that.isSkipped => isSkipped
                                    case that.isSkippedBecauseOfAFaultyMatcher => isSkippedBecauseOfAFaultyMatcher
                                    case that.isKo => failure1
                                    case that.isKoTwice => () => {failure1(); failure2()}
                                    case that.isKoWithTheFailMethod => failMethod
                                    case that.throwsAnException => exception
                                    case that.hasTwoSubExamples => subExamples
  }
}

class SpecWithOneExample(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "A specification" should {
       "have example 1 ok" in {
        expectations(behaviours) foreach {_.apply}
      }
    }
    reportSpecs
    messages
  }
}

class SpecWithTwoExamples(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "A specification" should {
      "have example 2.1 ok" in { expectations(behaviours).head.apply}
      "have example 2.2 ok" in { expectations(behaviours).last.apply }
    }
    reportSpecs
    messages
  }
}
class SpecWithTwoSystems extends TestSpecification {
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
class SpecWithLiterateDescription(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "The specification" is <p>
      Some text with {"embedded expectations" in {expectations(behaviours) foreach {_.apply}}}
    </p>
    reportSpecs
    messages
  }
}

object that extends Enumeration {
  val isKo, isOk, isKoTwice, isKoWithTheFailMethod,
      throwsAnException, isSkipped, isSkippedBecauseOfAFaultyMatcher,
      hasTwoSubExamples = Value
}
