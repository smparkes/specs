package org.specs.samples
import org.specs._

object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}
