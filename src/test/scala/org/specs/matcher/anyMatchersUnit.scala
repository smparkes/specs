package org.specs.matcher
import org.specs._
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._

class anyMatchersTest extends JUnit3(anyMatchersUnit)
object anyMatchersUnit extends MatchersSpecification {
  "A 'be' matcher" should {
    "be ok if comparing the same object" in {
      val name = "string"
      name must be(name)
    }
    "display a failure message if comparing different objects" in {
      assertion("name" must be("name2")) must failWith("'name' is not the same as 'name2'")
    }
    "be resilient to a null value" in {
      val s: String = null
      assertion("name" must be(s)) must failWith("'name' is not the same as 'null'")
    }
    "display a failure message if comparing different objects even if they are ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      assertion(o1 must be(o2)) must failWith("'MyObject(1)' is not the same as 'MyObject(1)'")
    }
    "be ok if comparing the same value" in {
      val number = 1
      number must be(number)
    }
    "display a failure message if comparing different values" in {
      assertion(1 must be(2)) must failWith("'1' is not the same as '2'")
    }
  }
  "An '==' matcher" should {
    "be ok if comparing 2 objects which are equals with ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      o1 must be_==(o2)
    }
    "display a failure message if comparing different objects" in {
      assertion("name" must be_==("name2")) must failWith("'name' is not equal to 'name2'")
    }
    "be resilient to a null value" in {
      val s: String = null
      assertion("name" must be_==(s)) must failWith("'name' is not equal to 'null'")
    }
  }
  "A 'beNull' matcher" should {
    "be ok if comparing with a null object" in {
      val a: String = null
      a must beNull
    }
    "display a failure message if the value is not null" in {
      val a: String = "not null"
      assertion(a must beNull) must failWith("'not null' is not null")
    }
  }
  "A 'notBeNull' matcher" should {
    "be ok if comparing with a non-null object" in {
      val a: String = ""
      a must notBeNull
    }
    "display a failure message if the value is null" in {
      val a: String = null
      assertion(a must notBeNull) must failWith("the value is null")
    }
  }
  "A 'beTrue' matcher" should {
    "be ok if comparing with a true object" in {
      true must beTrue
    }
    "display a failure message if the value is not true" in {
      assertion(false must beTrue) must failWith("'false' is false")
    }
  }
  "A 'beFalse' matcher" should {
    "be ok if comparing with a false object" in {
      false must beFalse
    }
    "display a failure message if the value is not true" in {
      assertion(true must beFalse) must failWith("'true' is true")
    }
  }
  "A throwA + exception matcher" should {
    "be ok if a value throws the expected exception type" in {
      throwA(new Error)(throw new Error("test")) must beLike { case (true, _, _) => ok } 
    }
    "be ko if the value doesn't throw any exception" in {
      throwA(new Exception)(1) must beLike { case (false, _, message) => ok } 
    }
    "specify the expected exception in the failure message" in {
      throwA(new Exception)(1)._3 must include((new Exception).getClass.getName) 
    }
    "throw a Failure exception if the value throws another exception" in {
      throwA(new Error)(throw new Exception) must throwA(new FailureException("")) 
    }
    "throw a Failure exception with the other exception message, if the value throws another exception" in {
      throwA(new Error("Error"))(throw new Exception) must throwThis(new FailureException("java.lang.Error: Error should have been thrown. Got: java.lang.Exception"))
    }
  }
  case class SimpleException(s: String) extends Exception(s)
  "A throwThis + exception matcher" should {
    "be ok if a value throws an exception equals to the expected exception one" in {
      throwThis(SimpleException("Message"))(throw SimpleException("Message"))._1 mustBe true 
    }
  }
  "A throwA + exception matcher" can {
    "specify a like clause to add pattern matching" in {
      throwA(SimpleException("Message")).like {case SimpleException(x) => !x.isEmpty}(
          throw new SimpleException("Message")) must beLike {case (true, _, _) => ok} 
    }
  }
  "Matchers" should {
    "not evaluate the expressions twice" in {
      case class exp[T](var a: T) { var evaluationsNb: Int= 0; def evaluate = {evaluationsNb += 1; a} }
      def mustEvalOnce[T <: S, S](a: exp[T], m: Matcher[S]) = { m.apply(a.evaluate); a.evaluationsNb mustBe 1 }
      mustEvalOnce(exp(Nil), beEmpty) 
      mustEvalOnce(exp(null), beNull)
      mustEvalOnce(exp(true), beTrue)
      mustEvalOnce(exp(false), beFalse)

      mustEvalOnce(exp(1), be_==(1))
      mustEvalOnce(exp(1), be(1))
      mustEvalOnce(exp(""), beIn(List(""))) 
      mustEvalOnce(exp(1), verify((x:Int) => x == 1))
      mustEvalOnce(exp(List(1)), contain(1))
      mustEvalOnce(exp(List(1)), exist((x:Int) => x > 0))
      mustEvalOnce(exp(List("")), existMatch(""))
      mustEvalOnce(exp(Map("" -> 1)), haveKey(""))
      mustEvalOnce(exp(Map("" -> 1)), haveValue(1))
      mustEvalOnce(exp(Map("" -> 1)), havePair("" -> 1))
      mustEvalOnce(exp(1), be_<(1))
      mustEvalOnce(exp(1), be_<=(1))
      mustEvalOnce(exp(1), be_>(1))
      mustEvalOnce(exp(1), be_>=(1))
      mustEvalOnce(exp(1), beCloseTo(1, 0))
      mustEvalOnce(exp(1), beLike { case 1 => ok })
      mustEvalOnce(exp(None), beNone)
      mustEvalOnce(exp(Some(1)), beSome[Int])
      mustEvalOnce(exp(""), equalIgnoreCase(""))
      mustEvalOnce(exp(""), beMatching(""))
      mustEvalOnce(exp(""), include(""))
      mustEvalOnce(exp(""), startWith(""))
      mustEvalOnce(exp(""), endWith(""))
      mustEvalOnce(exp(<b/>), \\("b"))
    }
  }
}
