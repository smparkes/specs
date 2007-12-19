package org.specs
import ExtendedThrowable._
import org.specs.runner._

object extendedThrowableUnitRunner extends ConsoleRunner(extendedThrowableUnit)
object extendedThrowableUnit extends Specification {
  "an extended Throwable" should {
    "provide a location method extracting the name of the file and the line from an exception" in {
      new Exception("hello").location must_== "extendedThrowableUnit.scala:9"
     }
      
    "accept xml" in {
      <toto>titi</toto> mustBe <toto>titi</toto>

      val n1 = <toto><foo/></toto>
      val n2 = <toto>
      <foo/>
      </toto>
     n1 must ==/(n2)
}

    
  }

}
