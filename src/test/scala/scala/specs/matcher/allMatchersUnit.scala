package scala.specs.matcher
import scala.collection.mutable.Queue
import scala.specs.runner._
import scala.specs.Sugar._

object allMatchersUnitSuite extends JUnit3(allMatchersUnit) 
object allMatchersUnit extends Specification {
  "Matchers unit tests" areSpecifiedBy (anyMatchersUnit, 
                                        iterableMatchersUnit,
                                        logicalMatchersUnit,
                                        stringMatchersUnit,
                                        numericMatchersUnit,
                                        patternMatchersUnit,
                                        scalacheckMatchersUnit,
                                        xmlMatchersUnit)
}
