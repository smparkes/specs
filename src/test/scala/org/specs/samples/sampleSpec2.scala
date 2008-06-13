package org.specs.samples
import org.specs._
import org.specs.specification._
import org.specs.Sugar._
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
import org.specs.mock._
import java.io._
object expectationsOnly extends Specification("Hello world") with JMocker with ClassMocker {
  "hello world".size mustBe 11
  3 must_== { "abc".size }
  classOf[OutputStream].expectsOne(_.flush) in { _.flush }
  "this example should also work" in { classOf[OutputStream].expectsOne(_.flush) in { _.flush} }
  
   val s = capturingParam[String]
   classOf[ToMock].expects(one(_).method0(s.must(beMatching("a")).capture) willReturn s) in { 
     _.method0("a")
   }
}
  class ToMock {
    def isEmpty = true
    def isEmpty2 = false
    def method0(p1: String) = p1
    def method1(p1: String, p2: String) = p1
    def method2(p1: String) = p1.size
    def method3(p1: String) = List(p1)
  }

class expectationsOnlyTest extends JUnit4(expectationsOnly)

