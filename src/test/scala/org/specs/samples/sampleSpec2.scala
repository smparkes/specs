package org.specs.samples

object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}