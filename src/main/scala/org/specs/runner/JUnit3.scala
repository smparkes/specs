package org.specs.runner
import org.specs.runner.javaConversions._
import org.specs.specification._
import _root_.junit.framework._
import _root_.org.junit.runner._
import org.specs.collection.JavaCollectionsConversion._

/**
 * This class is a JUnitTestSuite class with an empty test.<br>
 * JUnit 3 imposes that there is at least one test in a test suite in order to be able to run it
 * This class can run with JUnit4 thanks to the RunWith annotation and the custom JUnit38SuiteRunner
 */
class EmptyJUnit3TestSuite extends TestSuite

trait JUnit3TestSuite extends Test { 
  val testSuite = new EmptyJUnit3TestSuite
  def setName(n: java.lang.String): Unit = testSuite.setName(n)
  def getName: java.lang.String = testSuite.getName
  def addTest(t: Test) = testSuite.addTest(t)
  def run(result: TestResult) = testSuite.run(result)
  def countTestCases = testSuite.countTestCases
  def tests: java.util.Enumeration[Test] = testSuite.tests
  
  /** @return test suites nested in this suite */
  def suites = for(t <- tests; 
                   if (t.isInstanceOf[EmptyJUnit3TestSuite])) 
                 yield t.asInstanceOf[EmptyJUnit3TestSuite]

  /** @return test cases nested in this suite */
  def testCases = for(t <- tests; 
                    if (t.isInstanceOf[ExampleTestCase])) 
                 yield t.asInstanceOf[ExampleTestCase]
}

/**
 * This class is the base class for reporting the results of a Specification as a JUnit TestSuite<br>
 * Usage: <code>object mySuite extends JUnit3(mySpec1, mySpec2)</code><br>
 * If there is more than one specification, then the name of the suite is the name of the class: i.e mySuite
 * else, it is the description of the only specification
 * <p>
 * A nested junit TestSuite is created for each sub-specification and for each system under test
 */
@RunWith(classOf[JUnit38SuiteRunner])
class JUnit3(val specifications : Specification*) extends TestSuite with JUnit { 
  val specs: Seq[Specification] = specifications 
  override def setName(n: java.lang.String): Unit = super[JUnit].setName(n)
  override def addTest(t: Test) = super[JUnit].addTest(t)
  override def getName: java.lang.String = super[JUnit].getName
  override def tests: java.util.Enumeration[Test] = super[JUnit].tests
  override def run(result: TestResult) = super[JUnit].run(result)
  override def suites = super[JUnit].suites
  override def countTestCases = super[JUnit].countTestCases
}

trait JUnit extends JUnit3TestSuite with SpecsHolder {
  private var initialized = false
  override def run(result: TestResult) = {init; super.run(result)} 
  override def getName = {init; super.getName}
  override def tests: java.util.Enumeration[Test] = {init; super.tests}
  override def suites = {init; super.suites}
  override def countTestCases = {init; super.countTestCases}
  override def testCases = {init; super.testCases}
  override def addTest(t: Test) = {init; super.addTest(t)}

  private def init = { 
    if (!initialized) {
      if (specs.size > 1)
	    setName(this.getClass.getName.replaceAll("\\$", ""))
	  else
	    setName(specs(0).description)
	  specs foreach { specification => 
	    specification.subSpecifications.foreach {s: Specification => addTest(new JUnit3(s))}
	    if (specification.suts.size > 1) 
	      addTest(new SpecificationTestSuite(specification)) 
	    else  
	      specification.suts foreach {sut => addTest(new ExamplesTestSuite(sut.description + " " + sut.verb, sut.examples, sut.skippedSut))}
	  }
      initialized = true
    }
  }
}

/**
 * A <code>SpecificationTestSuite</code> is a junit TestSuite reporting the results of 
 * the suts of a Specification<br>
 * Its name is the description of the Specification
 */
class SpecificationTestSuite(specification: Specification) extends EmptyJUnit3TestSuite {
  setName(specification.description)
  specification.suts foreach {sut => addTest(new ExamplesTestSuite(sut.description + " " + sut.verb, sut.examples, sut.skippedSut))}
} 

/**
 * A <code>SpecificationTestSuite</code> is a junit TestSuite reporting the results of 
 * a list of examples. If an example has subExamples, they are reported with a separate <code>ExamplesTestSuite</code>
 */
class ExamplesTestSuite(description: String, examples: Iterable[Example], skipped: Option[Throwable]) extends EmptyJUnit3TestSuite {
  setName(description)
  examples foreach { example =>
    if (example.subExamples.isEmpty)
      addTest(new ExampleTestCase(example))
    else
      addTest(new ExamplesTestSuite(example.description, example.subExamples, skipped))
  }
  override def run(result: TestResult) = {
    skipped match {
      case Some(skipException) => result.addFailure(this, new SkippedAssertionError(skipException))
      case None => super.run(result)                                    
    }                                    
  }
}
/**
 * A <code>ExampleTestCase</code> reports the result of an example<p> 
 * It overrides the run method from <code>junit.framework.TestCase</code>
 * to add errors and failures to a <code>junit.framework.TestResult</code> object
 */
class ExampleTestCase(example: Example) extends TestCase(example.description.replaceAll("\n", " ")) { 
  override def run(result: TestResult) = {
      result.startTest(this)
      example.failures foreach {failure: FailureException => result.addFailure(this, new SpecAssertionFailedError(failure))}
      example.skipped foreach {skipped: SkippedException => result.addFailure(this, new SkippedAssertionError(skipped)) }
      example.errors foreach {error: Throwable => result.addError(this, new SpecAssertionFailedError(error)) }
      result.endTest(this)
  }
}
/**
 * This class refines the <code>AssertionFailedError</code> from junit 
 * and provides the stackTrace of an exception which occured during the specification
 * execution
 */
class SpecAssertionFailedError(t: Throwable) extends AssertionFailedError(t.getMessage){
  override def getStackTrace = t.getStackTrace
  override def printStackTrace = t.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}
class SkippedAssertionError(t: Throwable) extends SpecAssertionFailedError(t)
