package scala.specs.integration
import scala.specs.integration.javaConversions._

import junit.framework._

abstract class EmptyJUnit3TestSuite extends TestSuite { 
  def testEmpty = {} 
  def suites = for(t <- tests; if (t.isInstanceOf[EmptyJUnit3TestSuite])) yield t.asInstanceOf[EmptyJUnit3TestSuite]
  def testCases = for(t <- tests; if (t.isInstanceOf[JUnit3TestCase])) yield t.asInstanceOf[JUnit3TestCase]
}
class JUnit3(val specifications : Specification*) extends EmptyJUnit3TestSuite {
  if (specifications.size > 1)
    setName(this.getClass.getName.replaceAll("\\$", ""))
  else
    setName(specifications(0).getClass.getName.replaceAll("\\$", ""))
  specifications foreach { specification => 
    if (specification.suts.size > 1) 
      addTest(new SpecificationTestSuite(specification)) 
    else  
      specification.suts foreach {sut => addTest(new SpecTestSuite(sut.description + " " + sut.verb, sut.examples))}
  }
  
}
class SpecificationTestSuite(specification: Specification) extends EmptyJUnit3TestSuite {
  setName(specification.description)
  specification.suts foreach {sut => addTest(new SpecTestSuite(sut.description + " " + sut.verb, sut.examples))}
} 
class SpecTestSuite(description: String, examples: Iterable[Example]) extends EmptyJUnit3TestSuite {
  setName(description)
  examples foreach { example =>
    if (example.subExamples.isEmpty)
      addTest(new JUnit3TestCase(example.description.replaceAll("\n", " "), example.failures, example.errors))
    else
      addTest(new SpecTestSuite(example.description, example.subExamples))
  }
}
class JUnit3TestCase(description: String, failures: Iterable[FailureException], errors: Iterable[Throwable]) extends TestCase(description) { 
  override def run(result: TestResult) = {
      result.startTest(this)
      failures foreach {failure: FailureException => result.addFailure(this, new SpecAssertionFailedError(failure))}
      errors foreach {error: Throwable => result.addError(this, new SpecAssertionFailedError(error)) }
      result.endTest(this)
  }
}
class SpecAssertionFailedError(t: Throwable) extends AssertionFailedError(t.getMessage){
  override def getStackTrace = t.getStackTrace
  override def printStackTrace = t.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}
