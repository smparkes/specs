package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.runner.javaConversions._
import org.specs.runner._
import _root_.junit.framework._

object junit3TestSuiteRunner extends ConsoleRunner(junit3TestSuiteSpec)
class junit3TestSuiteTest extends JUnit3(junit3TestSuiteSpec)
object junit3TestSuiteSpec extends Specification {
  "A junit 3 test suite" should {
    "create a test suite containing tests suites for each specification " + 
    "in its constructor and a test case for each example" in {
      suite(that.isOk).getName must beMatching("SimpleSpec")
      suite(that.isOk).suites match {
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
    "report a failure with a stacktrace pointing to the assertion causing it in the executed specification" in {
      val result = new TestResult
      suite(that.isKo).run(result)
      result.failures verifies(_.hasMoreElements)
      val failure = result.failures.nextElement.asInstanceOf[TestFailure] 
      failure.exceptionMessage must_== "'ok' is not the same as 'first failure'"
      failure.trace.split("\n")(0) must include(failure.exceptionMessage)  
      failure.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("consoleReporterSpec.scala:\\d")) 
    }
    "report an error with a stacktrace indicating the location of the error in the specification" in {
      val result = new TestResult
      suite(that.throwsAnException).run(result)
      result.errors verifies(_.hasMoreElements)
      val error = result.errors.nextElement.asInstanceOf[TestFailure] 
      error.exceptionMessage must_== "new Error"
      error.trace.split("\n")(0) must include(error.exceptionMessage)
      error.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("consoleReporterSpec.scala:\\d"))
    }
  }
  def suite(behaviours: that.Value*) = new JUnit3(new SimpleSpec(behaviours.toList))
}

class SimpleSpec(behaviours: List[(that.Value)]) extends TestSpec {
  "A specification" should {
    "have example 1 ok" in {
      assertions(behaviours) foreach {_.apply}
    }
  }   
}

