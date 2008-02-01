package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.runner.javaConversions._
import org.specs.runner._
import org.specs.runner._
import org.specs.Sugar._
import org.scalatest._
import scala.collection.immutable._

class st extends ScalaTestSuite(scalaTestSpec)
object scalaTestSpecRunner extends ConsoleRunner(scalaTestSpec)
class scalaTestSpec extends JUnit3(scalaTestSpec)
object scalaTestSpec extends Specification {
  "A ScalaTest runner" should {
    "create a ScalaTest suite named after the specification description" in {
      val spec = new SimpleSpec(that.isOk)
      val suite = new ScalaTestSuite(spec)
      suite.suiteName must be_==(spec.description)
    }
    "create a ScalaTest suite named after the suite class name with no $ sign when created from several specs" in {
      val spec = new SimpleSpec(that.isOk)
      val suite = new ScalaTestSuite(spec, spec)
      suite.getClass.getName.replaceAll("\\$", "") must include(suite.suiteName)
    }
  }  
  /*    suite(that.isOk).suites match {
        case List() => fail("there should be a test suite")
        case ts::List() => {
  //        ts.getName mustMatch "A specification"
  //        ts.testCases match {
  //          case List() => fail("there should be a test case")
  //          case tc::List() => 
  //            tc.getName mustMatch "have example 1 ok"          
  //        }        }
    } */
    
  /*
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
    "report a skipped test" in {
      val result = new TestResult
      val listener = new RunNotifier { 
        var desc: Option[Description] = None 
        override def fireTestIgnored(d: Description) = desc = Some(d) 
      }
      result.addListener(new OldTestClassAdaptingListener(listener))
      suite(that.isSkipped).run(result)
      listener.desc must beSome[Description]
    }
    */
  def suite(behaviours: that.Value*) = new ScalaTestSuite(new SimpleSpec(behaviours.toList))
}  
  class SutSuite(sut: Sut) extends Suite {
    override def suiteName = sut.description
    override def nestedSuites: List[Suite] = Nil
    override def runTest(testName: java.lang.String, 
                         reporter: org.scalatest.Reporter, 
                         stopper: Stopper, 
                         properties: Map[java.lang.String, Any]): Unit = {
      sut.examples foreach {e =>
        reporter.testStarting(new Report(e.description, ""))
        e.failures foreach {f => reporter.testFailed(new Report(e.description, f.getMessage))}
        if (e.failures.isEmpty)
          reporter.testSucceeded(new Report(e.description, ""))
      }
      
    }
    override def testNames: Set[java.lang.String] = {
      var result: Set[String] = Set()
      sut.examples foreach {e => result = result + e.description}
      result
    }
    override def groups = Map()
      
  }
  class ScalaTestSuite(specifications: Specification*) extends Suite {
    override def suiteName = {
      if (specifications.size > 1)
        this.getClass.getName.replaceAll("\\$", "")
      else
        specifications(0).description
    }
    
    override def groups = Map()
    override def nestedSuites: List[Suite] = {
      var result: List[Suite] = Nil 
      specifications foreach { specification => 
        specification.subSpecifications.foreach { s: Specification => result = new ScalaTestSuite(s):::result }
        specification.suts foreach {sut => result = new SutSuite(sut):::result}
      }
      result
    }
  }
