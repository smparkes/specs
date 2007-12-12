package scala.specs
import org.specs.runner._
import org.specs.matcher._
import org.specs.Sugar._
import org.specs.matcher.MatcherUtils._
import org.specs.specification._

object specificationSuite extends JUnit3(specificationSpec)
object specificationSpec extends Specification { 
  "A specification" isSpecifiedBy (basicFeatures, advancedFeatures)
}

object basicFeatures extends SpecificationWithSamples {
  "A specification" should {
    "have a description being its unqualified class name by default" in { 
      object mySpec extends Specification
      mySpec.description must_== "mySpec"
    }
    "reference zero or more systems under test (sut)" in { 
      emptySpec.suts must beEmpty
      oneEx(that.isOk).suts.size mustBe 1
      twoSuts(that.isOk, that.isOk).suts.size mustBe 2
    }
    "have zero or more examples, sorted by sut" in {
      twoSuts(that.isOk, that.isOk).pretty must_== """twoSuts
                                                     |  This system under test should 
                                                     |    have example 1 ok
                                                     |  This other system under test should 
                                                     |    have example 1 ok""".stripMargin
    }
   "have no failures if it contains no assertion" in { 
     oneEx(that.isOk).failures must beEmpty
   } 
   "have one failure if one example fails" in { 
     oneEx(that.isKo).failures must beLike { case Seq(FailureException(_)) => ok } 
   } 
   "fail on the first example when having several failing examples" in { 
     oneEx(that.isKoTwice).failures must beLike { case Seq(FailureException(msg)) => msg must beMatching("first failure")} 
   } 
   "raise one error if one example throws an exception" in { 
     errorSpec.errors must beLike {case Seq(x: Throwable) => x.getMessage must_== "new Error"} 
   } 
   "provide the number of assertions" in { 
     twoSuts(that.isOk, List(that.isOk, that.isOk)).suts.map {_.assertionsNb} must_== List(1, 2)
     twoSuts(that.isOk, List(that.isOk, that.isOk)).assertionsNb mustBe 3
   } 
   "provide a 'fail' method adding a new failure to the current example" in {
     object failMethodSpec extends oneEx(List(that.isOk, that.isKoWithTheFailMethod))
     failMethodSpec.failures must beLike {case Seq(FailureException(msg)) => 
                                                 msg must_== "failure with the fail method"} 
   }
  }
}
object advancedFeatures extends SpecificationWithSamples {
  "A specification " can {
    "have a user-defined name" in {
      val spec = oneEx(that.isOk) 
      spec.name = "This is a great spec"
      spec.name must_== "This is a great spec"
    }
    "be composed of other specifications. The composite specification has subSpecifications.\n" + 
    "Use the isSpecifiedBy method to do so [alias areSpecifiedBy]."  in {
       object compositeSpec extends Specification {
         "A complex system" isSpecifiedBy (okSpec, koSpec)
       }
       compositeSpec.description must_== "A complex system is specified by"
       compositeSpec.subSpecifications must_== List(okSpec, koSpec)
    }
    "share examples with another specficiation.\n" +
    "Declare an example to be a collection of examples coming from another spec. " +
    "The specified example will have the other examples as sub-examples" in {
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
}

trait SpecificationWithSamples extends Specification {
 
  abstract class TestSpec extends Specification {
    val success = () => true mustBe true
    val failure1 = () => "ok" mustBe "first failure"
    val failure2 = () => "ok" mustBe "second failure"
    val failMethod = () => fail("failure with the fail method")
    val exception = () => error("new Error")
    def assertions(behaviours: List[that.Value]) = behaviours map { case that.isOk => success
                                      case that.isKo => failure1
                                      case that.isKoTwice => () => {failure1(); failure2()} 
                                      case that.isKoWithTheFailMethod => failMethod 
                                      case that.throwsAnException => exception }
  }
  object specification extends Specification
  object okSpec extends oneEx(that.isOk)
  object koSpec extends oneEx(that.isKo)
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
}
object that extends Enumeration {
  val isKo, isOk, isKoTwice, isKoWithTheFailMethod, throwsAnException = Value
}



