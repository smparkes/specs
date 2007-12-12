package scala.specs.samples

object sampleSpec1 extends Specification {
  "A sample specification1" should {
    "return something" in {
       "hello" mustNotBe "world"
    }
  }
}