package scala.specs.runner
import scala.specs.runner.javaConversions._
import scala.specs.specification._
import junit.framework._

/**
 * This class is a JUnitTestSuite class with an empty test.
 * JUnit 3 imposes that there is at least one test in a test suite in order to be able to run it
 */
abstract class EmptyJUnit3TestSuite extends TestSuite { 
  
  /** defines an empty test to satisfy junit requirements for test cases */
  def testEmpty = {} 

  /** returns test suites nested in this suite */
  def suites = for(t <- tests; 
                   if (t.isInstanceOf[EmptyJUnit3TestSuite])) 
                 yield t.asInstanceOf[EmptyJUnit3TestSuite]

  /** returns test cases nested in this suite */
  def testCases = for(t <- tests; 
                    if (t.isInstanceOf[ExampleTestCase])) 
                 yield t.asInstanceOf[ExampleTestCase]
}

/**
 * This class is the base class for reporting the results of a Specification as a JUnit TestSuite
 * Usage: <code>object mySuite extends JUnit3(mySpec1, mySpec2)</code>
 * If there is more than one specification, then the name of the suite is the name of the class: i.e mySuite
 * else, it is the description of the only specification
 * 
 * A nested junit TestSuite is created for each sub-specification and for each system under test
 */
class JUnit3(val specifications : Specification*) extends EmptyJUnit3TestSuite {
  if (specifications.size > 1)
    setName(this.getClass.getName.replaceAll("\\$", ""))
  else
    setName(specifications(0).description)
  specifications foreach { specification => 
    specification.subSpecifications.foreach {s: Specification => addTest(new JUnit3(s))}
    if (specification.suts.size > 1) 
      addTest(new SpecificationTestSuite(specification)) 
    else  
      specification.suts foreach {sut => addTest(new ExamplesTestSuite(sut.description + " " + sut.verb, sut.examples))}
  }
  
}

/**
 * A <code>SpecificationTestSuite</code> is a junit TestSuite reporting the results of 
 * the suts of a Specification
 * Its name is the description of the Specification
 */
class SpecificationTestSuite(specification: Specification) extends EmptyJUnit3TestSuite {
  setName(specification.description)
  specification.suts foreach {sut => addTest(new ExamplesTestSuite(sut.description + " " + sut.verb, sut.examples))}
} 

/**
 * A <code>SpecificationTestSuite</code> is a junit TestSuite reporting the results of 
 * a list of examples. If an example has subExamples, they are reported with a separate <code>ExamplesTestSuite</code>
 */
class ExamplesTestSuite(description: String, examples: Iterable[Example]) extends EmptyJUnit3TestSuite {
  setName(description)
  examples foreach { example =>
    if (example.subExamples.isEmpty)
      addTest(new ExampleTestCase(example))
    else
      addTest(new ExamplesTestSuite(example.description, example.subExamples))
  }
}
/**
 * A <code>ExampleTestCase</code> reports the result of an example 
 * It overrides the run method from <code>junit.framework.TestCase</code>
 * to add errors and failures to a <code>junit.framework.TestResult</code> object
 */
class ExampleTestCase(example: Example) extends TestCase(example.description.replaceAll("\n", " ")) { 
  override def run(result: TestResult) = {
      result.startTest(this)
      example.failures foreach {failure: FailureException => result.addFailure(this, new SpecAssertionFailedError(failure))}
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
