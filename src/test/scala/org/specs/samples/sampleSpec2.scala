package org.specs.samples
import org.specs._
import org.specs.runner._

object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}
object AllTests extends Specification {
  isSpecifiedBy(sampleSpec2, sampleSpec2)
}