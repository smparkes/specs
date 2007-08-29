package scala.specs
import scala.specs.integration._
import scala.specs.matchers._
import scala.specs.mock._

object allSpecsSuite extends JUnit3(allSpecs)
object allSpecs extends Specification {
    "The specifications for the specs project" areSpecifiedBy (
    matchersSpec, 
    specificationSpec, 
    consoleReporterSpec,  
    beforeAfterSpec, 
    specsFinderSpec,
    stackSpecification,
    junit3TestSuiteSpec,
    mocksSpec)
}

object allUnits extends Specification {
  "The unit tests for the specs projects" areSpecifiedBy (
      specificationUnit, 
      protocolsUnit)
}

object all extends JUnit3(
  allSpecs, 
  allUnits)