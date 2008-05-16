package org.specs.samples
import org.specs._
object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}
import org.specs.runner._
class helloWorldTest extends JUnit4(helloWorld)
object helloWorld extends Specification("Hello world") {
  "'hello world' has 11 characters" in {
     "hello world".size mustBe 11
  }
  "'hello world' matches 'h.* w.*'" in {
     "hello world" must beMatching("h.* w.*")
  }
}
