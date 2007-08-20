package scala.specs

import scala.specs._
import scala.specs.Sugar._
import scala.specs.integration._
import scala.util._
import scala.collection.mutable._
import scala.io.mock.MockOutput
import scalacheck.Gen._

object specificationSuite extends JUnit3TestSuite(specificationSpec)
object specificationSpec extends Specification {

  "A specification" should {
    "have a description being its unqualified class name by default" in { 
      okSpec.description must_== "okSpec"
      object internalSpec extends Specification
      internalSpec.description must_== "internalSpec"
    }
    "reference zero or more systems under test (sut)" in { 
      emptySpec.suts must beEmpty
      okSpec.suts.size mustBe 1
      sut1OkSut2Ok.suts.size mustBe 2
    }
    "have zero or more examples, sorted by sut" in {
      specWithoutExample.suts.flatMap{_.examples} must beEmpty
      okSpec.suts.flatMap{_.examples}.size mustBe 1
      sut1OkSut2Ok.suts.flatMap{_.examples}.size mustBe 2
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
      object compositeSpec extends TestSpec with SharedExamples {
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
         packageName <- elements("com", "scala")
         className <- elements(packageName + "s", packageName + ".specs", packageName + ".other.normal")
         name <- elements(className, className + "$inner", className + "$inner$", className + "$2", className + "$2$")
       } yield name

       def property = (className : String) => specification.
         createDescription(className) must (not(beMatching("\\$")) and 
                                            not(beMatching("\\.")) and
                                            not(beInt))
       property must pass(classNames)
    }
    "have no failures it contains no assertion" in { 
      okSpec.failures must beEmpty
    } 
    "have one failure if one example fails" in { 
      koSpec.failures must beLike {case Seq(FailureException(_)) => true} 
    } 
    "have only one failure for an example failing twice" in { 
      koKoSpec.failures.size mustBe 1 
    } 
    "have one error if one example throws an exception" in { 
      errorSpec.errors must beLike {case Seq(x: Throwable) => x.getMessage must_== "new Error"} 
    } 
    "count the number of assertions" in { 
      sut1OkSut2OkOk.suts must beLike {case Seq(sut1: Sut, sut2: Sut) => 
                                                  (sut1.assertionsNb must_== 1) &&
                                                  (sut2.assertionsNb must_== 2)} 
      sut1OkSut2OkOk.assertionsNb mustBe 3
    } 
    "have a 'fail' method adding a new failure to the last example" in {
      object failMethodSpec extends SpecWithOneExample(List(that.succeeds, that.failsWithTheFailMethod))
      failMethodSpec.failures must beLike {case Seq(FailureException(msg)) => 
                                                  msg must_== "failure with the fail method"} 
    }
  }
  
  def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def beInt = Matcher[String](s => (isInt(s), q(s) + " is an integer", q(s) + " is not an integer"))
 
  abstract class TestSpec extends Specification with ConsoleReporter with MockOutput {
    val ok = () => true mustBe true
    val failure1 = () => "ok" mustBe "first failure"
    val failure2 = () => "ok" mustBe "second failure"
    val failMethod = () => fail("failure with the fail method")
    val exception = () => throw new Error("new Error")
    def assertions(behaviours: List[that.Value]) = behaviours map { case that.succeeds => ok
                                      case that.fails => failure1
                                      case that.failsTwice => failure2 
                                      case that.failsWithTheFailMethod => failMethod 
                                      case that.throwsAnException => exception }
  }
  object specification extends Specification
  object emptySpec extends TestSpec
  object okSpec extends SpecWithOneExample(that.succeeds)
  object koSpec extends SpecWithOneExample(that.fails)
  object koKoSpec extends SpecWithOneExample(List(that.fails, that.fails))
  object errorSpec extends SpecWithOneExample(that.throwsAnException)
  object ko1ko1Spec extends SpecWithTwoExamples(that.fails, that.fails)
  object sut1OkSut2Ok extends SpecWithTwoSuts(that.succeeds, that.succeeds)
  object sut1OkSut2OkOk extends SpecWithTwoSuts(that.succeeds, List(that.succeeds, that.succeeds))
  object specWithoutExample extends TestSpec {
    "A system under test" should { /* no example yet */ }
  }
  object compositeSpec extends TestSpec {
    "A system under test" should {
      "have examples coming from another spec" in {
        okSpec.suts.flatMap{_.examples}
      }
    }
  }
  class SpecWithOneExample(behaviours: List[(that.Value)]) extends TestSpec {
    "A system under test" should {
      "have example 1 ok" in {
        assertions(behaviours) foreach {_.apply}
      }
    }
  }
  class SpecWithTwoExamples(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "A system under test" should {
      "have example 2.1 ok" in { assertions(behaviours1).head.apply}
      "have example 2.2 ok" in { assertions(behaviours2).last.apply }
    }
  }
  class SpecWithTwoSuts(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "A system under test" should {
      "have example 1 ok" in {
        assertions(behaviours1) foreach {_.apply}
      }
    }
    "A system under test" should {
      "have example 1 ok" in {
        assertions(behaviours2) foreach {_.apply}
      }
    }
  }

  object that extends Enumeration {
    val fails, succeeds, failsTwice, failsWithTheFailMethod, throwsAnException = Value
  }
}



