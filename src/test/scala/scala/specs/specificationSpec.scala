package scala.specs

import scala.specs._
import scala.specs.Sugar._
import scala.specs.integration._
import scala.util._
import scala.collection.mutable._
import scalacheck.Gen._

object specificationSuite extends JUnit3TestSuite(specificationSpec)
object specificationSpec extends Specification {

  "A specification" should {
    "have a description being its unqualified class name by default" in { 
      oneEx(that.isOk).description must_== "mySpec"

      object internalSpec extends Specification
      internalSpec.description must_== "internalSpec"
    }
    "reference zero or more systems under test (sut)" in { 
      emptySpec.suts must beEmpty
      oneEx(that.isOk).suts.size mustBe 1
      twoSuts(that.isOk, that.isOk).suts.size mustBe 2
    }
    "have zero or more examples, sorted by sut" in {
      specWithoutExample.suts.flatMap{_.examples} must beEmpty
      oneEx(that.isOk).suts.flatMap{_.examples}.size mustBe 1
      twoSuts(that.isOk, that.isOk).suts.flatMap{_.examples}.size mustBe 2
    }
  }
  "A specification " can {
    "have a user-defined description" in {
      oneEx(that.isOk).description = "This is a great spec"
      oneEx(that.isOk).description must_== "This is a great spec"
    }
    "be composed of other specifications: 'mySpec isSpecifiedBy (s1, s2, s3)' " in {
       object compositeSpec extends Specification {
         "A composite spec" isSpecifiedBy (oneEx(that.isOk), oneEx(that.isOk))
       }
       compositeSpec.description must_== "A composite spec is specified by"
       compositeSpec.suts.size mustBe 2
       compositeSpec.suts must beLike { case x::y::Nil => (x, y) == (oneEx(that.isOk).suts.head, 
                                                                     oneEx(that.isOk).suts.head) }
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
      oneEx(that.isOk).failures must beEmpty
    } 
    "have one failure if one example isKo" in { 
      oneEx(that.isKo).failures must beLike {case Seq(FailureException(_)) => true} 
    } 
    "have only one failure for an example failing twice" in { 
      oneEx(List(that.isKo, that.isKo)).failures.size mustBe 1 
    } 
    "have one error if one example throws an exception" in { 
      errorSpec.errors must beLike {case Seq(x: Throwable) => x.getMessage must_== "new Error"} 
    } 
    "count the number of assertions" in { 
      twoSuts(that.isOk, List(that.isOk, that.isOk)).suts must beLike {case Seq(sut1: Sut, sut2: Sut) => 
                                                  (sut1.assertionsNb must_== 1) &&
                                                  (sut2.assertionsNb must_== 2)} 
      twoSuts(that.isOk, List(that.isOk, that.isOk)).assertionsNb mustBe 3
    } 
    "have a 'fail' method adding a new failure to the last example" in {
      object failMethodSpec extends oneEx(List(that.isOk, that.isKoWithTheFailMethod))
      failMethodSpec.failures must beLike {case Seq(FailureException(msg)) => 
                                                  msg must_== "failure with the fail method"} 
    }
  }
  
  def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def beInt = Matcher[String](s => (isInt(s), q(s) + " is an integer", q(s) + " is not an integer"))
 
  abstract class TestSpec extends Specification {
    val ok = () => true mustBe true
    val failure1 = () => "ok" mustBe "first failure"
    val failure2 = () => "ok" mustBe "second failure"
    val failMethod = () => fail("failure with the fail method")
    val exception = () => throw new Error("new Error")
    def assertions(behaviours: List[that.Value]) = behaviours map { case that.isOk => ok
                                      case that.isKo => failure1
                                      case that.isKoTwice => failure2 
                                      case that.isKoWithTheFailMethod => failMethod 
                                      case that.throwsAnException => exception }
  }
  object specification extends Specification
  object emptySpec extends TestSpec
  object errorSpec extends oneEx(that.throwsAnException)
  object specWithoutExample extends TestSpec {
    "A system under test" should { /* no example yet */ }
  }
  object compositeSpec extends TestSpec {
    "This composite spec" should {
      "take its examples from another spec" in {
        oneEx(that.isOk).suts.flatMap{_.examples}
      }
    }
  }
  case class oneEx(behaviours: List[(that.Value)]) extends TestSpec {
    "This system under test" should {
      "have example 1 ok" in {
        assertions(behaviours) foreach {_.apply}
      }
    }
  }
  case class SpecWithTwoEx(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "This system under test" should {
      "have example 2.1 ok" in { assertions(behaviours1).head.apply}
      "have example 2.2 ok" in { assertions(behaviours2).last.apply }
    }
  }
  case class twoSuts(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "This system under test" should {
      "have example 1 ok" in {
        assertions(behaviours1) foreach {_.apply}
      }
    }
    "This other system under test" should {
      "have example 1 ok" in {
        assertions(behaviours2) foreach {_.apply}
      }
    }
  }
  object that extends Enumeration {
    val isKo, isOk, isKoTwice, isKoWithTheFailMethod, throwsAnException = Value
  }
}



