package org.specs.runner
import org.scalatest._

/**
 * This class is a ScalaTest suite which is build from a specification and return as nested suites
 * its subspecifications or its systems under test (sut)
 */
class ScalaTestSuite(specifications: Specification*) extends org.scalatest.Suite {
  /**
   * @return the name of the suite which is either the specification name if there's only one or 
   * a name build after <code>this</code> class
   */
  override def suiteName = {
    if (specifications.size > 1)
      this.getClass.getName.replaceAll("\\$", "")
    else
      specifications(0).description
  }
    
  /**
   * @return an empty map for now. The notion of group may be added later to specifications
   */
  override def groups = Map()

  /**
   * @return the subspecifications or the suts as ScalaTest suites
   */
  override def nestedSuites: List[org.scalatest.Suite] = {
    var result: List[org.scalatest.Suite] = Nil 
    specifications foreach { specification => 
      specification.subSpecifications.foreach { s: Specification => result = new ScalaTestSuite(s)::result }
      specification.suts foreach {sut => result = new SutSuite(sut)::result }
    }
    result
  }
}

/**
 * This class is a ScalaTest suite which is build from a system under test
 * its subspecifications or its systems under test (sut)
 */
class SutSuite(sut: Sut) extends Suite {
  
  /**
   * @return the description of the sut with either "should" or "can"
   */
  override def suiteName = sut.description + " " + sut.verb

  /**
   * @return Nil. A system under test has no nested suites
   */
  override def nestedSuites: List[Suite] = Nil

  /**
   * @return the descriptions of the examples to report. Subexamples names are not returned and will be run with their parent example
   */
  override def testNames: Set[java.lang.String] = {
    var result: Set[String] = Set()
     sut.examples foreach {e => result = result + e.description}
     result
  }

  /**
   * Report the result of an example given its description to the reporter.  
   */
  override def runTest(testName: java.lang.String, 
                         reporter: org.scalatest.Reporter, 
                         stopper: Stopper, 
                         properties: Map[java.lang.String, Any]): Unit = {
      val example = sut.examples find {_.description == testName}
      example match {
        case Some(e) => runExample(e, reporter)      
        case None => ()
      }
    }
    
  /**
   * Report the result of an example: ignored if it is skipped, failed if it has failures or errors, succeeded otherwise
   * call this method recursively if the example has subexamples
   */
  private[this] def runExample(e: Example, reporter: org.scalatest.Reporter): Unit = {
    reporter.testStarting(new Report(e.description, ""))
    e.skipped foreach {skipped => reporter.testIgnored(new Report(e.description, skipped.message))}
    e.failures foreach {f => reporter.testFailed(new Report(e.description, f.getMessage))}
    e.errors foreach {error => reporter.testFailed(new Report(e.description, error.getMessage, Some(error), None))}
    if (e.failures.isEmpty)
      reporter.testSucceeded(new Report(e.description, ""))
    e.subExamples foreach { sub => runExample(sub, reporter) }
  }
    
  /**
   * @return an empty map for now. The notion of group may be added later to specifications
   */
  override def groups = Map()
      
}
