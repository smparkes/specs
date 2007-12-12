package scala.util

import org.specs.runner._
import org.specs._


object allUtilSuite extends JUnit3(allUtilSpec)
object allUtilSpec extends Specification {
    "The util specifications" areSpecifiedBy (
        timerSpec)
}

object allUtilUnit extends Specification {
  "The unit tests for the util package" areSpecifiedBy (
      dataRowUnit, 
      dataTableHeaderUnit,  
      dataTableUnit)
}

