package org.specs.samples
import org.specs._

object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}
object helloWorld extends Specification {
  "return 11 characters for hello world" in {
     "hello world".size mustBe 12
  }
}
