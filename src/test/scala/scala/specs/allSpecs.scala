package scala.specs
import scala.specs.integration._

object allSpecs extends JUnit3TestSuite(
    assertSpec, 
    specificationSpec, 
    consoleReporterSpec,  
    beforeAfterSpec, 
    specsFinderSpec,
    stackSpecification,
    junit3TestSuiteSpec)