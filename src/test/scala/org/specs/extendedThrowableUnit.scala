package org.specs
import ExtendedThrowable._
import org.specs.runner._

object extendedThrowableUnitRunner extends ConsoleRunner(extendedThrowableUnit)
object extendedThrowableUnit extends Specification {
  "an extended Throwable" should {
    "provide a location method extracting the name of the file and the line from an exception" in {
      new Exception("hello").location must_== "extendedThrowableUnit.scala:9"
     }
  }
}
