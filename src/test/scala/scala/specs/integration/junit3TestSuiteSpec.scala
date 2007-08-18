package scala.specs.integration
import scala.io.mock.MockOutput
import junit.framework._
import scala.specs.integration.javaConversions._

object junit3TestSuiteTestSuite extends JUnit3TestSuite(junit3TestSuiteSpec)
object junit3TestSuiteSpec extends Specification {
  "A junit 3 test suite" should {
    "create a test suite containing tests suites for each specification " + 
    "in its constructor and a test case for each example" in {
      suite(that.succeeds).getName must beMatching("SpecWithOneExample")
      suite(that.succeeds).suites match {
        case List() => fail("there should be a test suite")
        case ts::List() => {
          ts.getName mustMatch "A specification"
          ts.testCases match {
            case List() => fail("there should be a test case")
            case tc::List() => 
              tc.getName mustMatch "have example 1 ok"
          }
        }
      }
    }
  }
  "report a failure with a stacktrace pointing to the assertion causing it in the executed specification" in {
    val result = new TestResult
    suite(that.fails).run(result)
    result.failures verifies {_.hasMoreElements}
    val failure = result.failures.nextElement.asInstanceOf[TestFailure] 
    failure.exceptionMessage must_== "'ok' is not the same as 'first failure'"
    failure.trace.split("\n")(0) must include(failure.exceptionMessage)  
    failure.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("junit3TestSuiteSpec.scala:\\d")) 
  }
  "report an error with a stacktrace indicating the location of the error in the specification" in {
    val result = new TestResult
    suite(that.throwsAnException).run(result)
    result.errors verifies {_.hasMoreElements}
    val error = result.errors.nextElement.asInstanceOf[TestFailure] 
    error.exceptionMessage must_== "new Error"
    error.trace.split("\n")(0) must include(error.exceptionMessage)
    error.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("junit3TestSuiteSpec.scala:\\d"))
  }
  def suite(behaviours: that.Value*) = new JUnit3TestSuite(new SpecWithOneExample(behaviours.toList))
}


abstract class TestSpec(behaviours: List[that.Value]) extends Specification with ConsoleReporter with MockOutput {
  val ok = () => true mustBe true
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val exception = () => throw new Error("new Error")
  def assertions = behaviours map { case that.succeeds => ok
                                    case that.fails => failure1
                                    case that.failsTwice => failure2 
                                    case that.throwsAnException => exception }
}

class SpecWithOneExample(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
    "A specification" should {
      "have example 1 ok" in {
        assertions foreach {_.apply}
      }
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
  val fails, succeeds, failsTwice, throwsAnException = Value
}

