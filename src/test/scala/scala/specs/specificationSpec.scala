package scala.specs

import scala.specs._
import scala.specs.Sugar._
import scala.specs.integration._
import scala.util._
import scala.collection.mutable._
import scala.io.mock.MockOutput
import junit.framework._
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._
import scala.specs.integration.sCheck._

object specificationSuite extends JUnit3TestSuite(specificationSpec)
object specificationSpec extends Specification with Sugar {

  "A specification" should {
    "have a description being its unqualified class name by default" in { 
      okSpec.description must_== "okSpec"
      object internalSpec extends Specification
      internalSpec.description must_== "internalSpec"
    }
    "reference zero or more systems under test (sut)" in { 
      emptySpec.suts must beEmpty
      okSpec.suts.size mustBe 1
      specWithTwoSuts.suts.size mustBe 2
    }
    "have zero or more examples, sorted by sut" in {
      specWithoutExample.suts.flatMap{_.examples} must beEmpty
      okSpec.suts.flatMap{_.examples}.size mustBe 1
      specWithTwoSuts.suts.flatMap{_.examples}.size mustBe 2
    }
  }
  "A specification " can {
    "have a user-defined description" in {
      okSpec.description = "This is a great spec"
      okSpec.description must_== "This is a great spec"
    }
    "be composed of other specifications: 'mySpec isSpecifiedBy (s1, s2, s3)' " in {
       object compositeSpec extends Specification {
         "A composite spec" isSpecifiedBy (okSpec, okSpec)
       }
       compositeSpec.description must_== "A composite spec is specified by"
       compositeSpec.suts.size mustBe 2
       compositeSpec.suts must beLike { case x::y::Nil => (x, y) == (okSpec.suts.head, okSpec.suts.head) }
    }
  }
  "A specification with one sut" can {
    "share examples with another spec: 'use those examples' in otherExamples " in {
      trait SharedExamples extends Specification {
        def sharedExamples = {
          "this is a new example" in { 1 mustBe 1 }
        }
      }
      object compositeSpec extends TestSpec(Nil) with SharedExamples {
        "A system under test" should { "share examples with another spec" in sharedExamples }
      }
      compositeSpec.description must_== "compositeSpec"
      compositeSpec.suts.head.examples must beLike {
        case Seq(ex: Example) => 
          ex.subExamples must beLike { case Seq(subEx) => true }
      }
    }
  }
  "A specification" should {
    "have a description corresponding to its unqualified class name, whatever the class name" in {
       def classNames = for {
         packageName <- choose("com", "scala")
         className <- choose(packageName + "s", packageName + ".specs", packageName + ".other.normal")
         name <- choose(className, className + "$inner", className + "$inner$", className + "$2", className + "$2$")
       } yield CombinedString(name)

       object specification extends Specification
       def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
       def beInt = Matcher[String](s => (isInt(s), q(s) + " is an integer", q(s) + " is not an integer"))
       def property = (className : CombinedString) => 
         specification.createDescription(className) must (not(beMatching("\\$")) and 
                                                          not(beMatching("\\.")) and
                                                          not(beInt))
       property must pass(classNames)
    }
  }
  
    /*
    "display '0 failure' if there is no assertion" in { 
      specWithOneExample(that.succeeds) must existMatch("0 failure")
    } 
    "display '1 failure' if one example fails" in { 
      specWithOneExample(that.fails) must existMatch("1 failure") 
    } 
    "display the first failure of an example having several ones" in { 
      specWithOneExample(that.fails, that.fails) must existMatch("first failure") 
      specWithOneExample(that.fails, that.fails) must notExistMatch("second failure")
    } 
    "display '1 error' if one example throws an exception" in { 
      specWithOneExample(that.throwsAnException) must existMatch("1 error") 
    } 
    "report a pluralized message if there are several examples failing" in { 
      specWithTwoExamples(that.fails) must existMatch("2 examples")
      specWithTwoExamples(that.fails) must existMatch("2 failures")
    } 
    "report the number of assertions: '2 assertions'" in { 
      specWithOneExample(that.succeeds) must existMatch("1 assertion")
      specWithTwoExamples(that.fails) must existMatch("2 assertions")
    } 
    "display the failure message next to the corresponding example" in { 
      specWithTwoExamples(that.fails, that.succeeds) verifies (messages =>
            messages.findIndexOf(matches("first failure")) ==
            messages.findIndexOf(matches("example 2.1 ok")) + 1)
    } 
    "report the elapsed time" in { 
      specWithOneExample(that.succeeds) mustExistMatch "Finished in"
    }
    "have a 'fail' method adding a new failure to the last example" in {
      specWithOneExample(that.failsWithTheFailMethod) mustExistMatch "1 failure" 
    }
    */
  

  def specWithOneExample(assertions: (that.Value)*) = new SpecWithOneExample(assertions.toList)
  def specWithTwoExamples(assertions: (that.Value)*) = new SpecWithTwoExamples(assertions.toList)

  abstract class TestSpec(behaviours: List[that.Value]) extends Specification with ConsoleReporter with MockOutput {
    val ok = () => true mustBe true
    val failure1 = () => "ok" mustBe "first failure"
    val failure2 = () => "ok" mustBe "second failure"
    val failMethod = () => fail("failure with the fail method")
    val exception = () => throw new Error("new Error")
    def assertions = behaviours map { case that.succeeds => ok
                                      case that.fails => failure1
                                      case that.failsTwice => failure2 
                                      case that.failsWithTheFailMethod => failMethod 
                                      case that.throwsAnException => exception }
  }
  object emptySpec extends TestSpec(Nil)
  object specWithTwoSuts extends SpecWithTwoSuts(that.succeeds)
  object okSpec extends SpecWithOneExample(that.succeeds)
  object specWithoutExample extends TestSpec(Nil) {
    "A system under test" should {
    // no example yet
    }
  }
  object compositeSpec extends TestSpec(Nil) {
    "A system under test" should {
      "have examples coming from another spec" in {
        okSpec.suts.flatMap{_.examples}
      }
    }
  }
  class SpecWithOneExample(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
    "A system under test" should {
      "have example 1 ok" in {
        assertions foreach {_.apply}
      }
    }
  }
  class SpecWithTwoExamples(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
    "A system under test" should {
      "have example 2.1 ok" in { assertions.head.apply}
      "have example 2.2 ok" in { assertions.last.apply }
    }
  }
  class SpecWithTwoSuts(behaviour: List[(that.Value)]) extends TestSpec(behaviour) {
    "A system under test" should {
      "have example 1 ok" in {
        assertions foreach {_.apply}
      }
    }
    "A system under test" should {
      "have example 1 ok" in {
        assertions foreach {_.apply}
      }
    }
  }

  object that extends Enumeration {
    val fails, succeeds, failsTwice, failsWithTheFailMethod, throwsAnException = Value
  }
}



