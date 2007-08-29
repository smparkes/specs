package scala.specs
import scala.specs.integration._
import scala.specs.matchers._

object allSpecs extends JUnit3TestSuite(
    matchersSpec, 
    specificationSpec, 
    consoleReporterSpec,  
    beforeAfterSpec, 
    specsFinderSpec,
    stackSpecification,
    junit3TestSuiteSpec,
    mockProtocols)