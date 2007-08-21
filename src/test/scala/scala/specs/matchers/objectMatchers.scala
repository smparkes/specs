package scala.specs.matchers
import scala.collection.mutable.Queue
import scala.specs.integration._

object objectMatchersTestSuite extends JUnit3TestSuite(objectMatchers) 
object objectMatchers extends MatchersSpecification {
  "Object matchers" should { usingBefore { () => clearExample }
    "provide a 'must_==' matcher: 'name' must_== 'name'" in {
      "string" must_== "string"
      assertion("string" must_== "string2") must failWith("'string' is not equal to 'string2'")  
    }
    "provide a 'must_!=' matcher 'name' must_!= 'name2'" in {
      "string" must_!= "string2"
      assertion("string" must_!= "string") must failWith("'string' is equal to 'string'")
    }
  }
  "provide a 'must be' matcher: o1 must be(o2) if they are the same object " + 
    "('must eq' cannot be used because it overrides the eq matcher from Object) [alias: mustBe, mustEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1
        o1 must be(o2)
        o1 mustBe o2
        o1 mustEq o2
        assertion(o1 must be(o3)) must failWith("'o1' is not the same as 'o3'")
    }
    "provide a 'must notBe' matcher: o1 must notBe(o2) if they are not the same object [alias: mustNotBe, mustNotEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1

        o1 must notBe(o3)
        o1 mustNotBe o3
        o1 mustNotEq o3
        assertion(o1 must notBe(o2)) must failWith("'o1' is the same as 'o1'")
    }
    "provide a 'verifies' matcher checking if an object verifies a property: 'name' verifies {_.size == 4} [alias: verify]" in {
      List() verifies { _.isEmpty }
      List() verify { _.isEmpty }
      assertion(List("1") verifies { _.isEmpty }) must failWith("List(1) doesn't verify the expected property")
    }
    "provide a 'mustThrow' matcher expecting a block to send an exception of a given type" in {
      {throw new Error("user error"); ()} mustThrow new Error()

      class MyError(msg: String) extends Error(msg) {}
      {throw new MyError("subclass of error"); ()} mustThrow new Error() 

      assertion({throw new NullPointerException; ()} mustThrow new Error()) must failWith("java.lang.Error should have been thrown. Got: java.lang.NullPointerException")
    } 
}
object patternMatchers extends MatchersSpecification {
  "Pattern matchers" should { usingBefore { () => clearExample }
    "provide a beLike matcher using pattern matching: (1, 2) must beLike {case (1, _) => ok} " +
    "[ok is syntactic sugar for true from the Sugar trait]" in {
      "a" must beLike {case "a" => ok}
      ("a", "b") must beLike {case ("a", _) => ok}
      ("a", "b", "c") must beLike {case ("a", _, _) => ok}
      assertion(("a", "b", "c") must beLike {case ("a", _) => ok}) must 
                       failWith ("(a,b,c) doesn't match the expected pattern")
      assertion(("a", "b", "c") must not(beLike {case ("a", _, _) => ok})) must 
           failWith ("(a,b,c) matches the given pattern")
    }
    "provide a beNone matcher for options: List().find {_ == 2} must beNone" in {
      List().find {_ == 2} must beNone
      assertion(List(2).find {_ == 2} must beNone[Int]) must failWith("Some(2) is not None")
    }
    "provide a beSome matcher for options: List(2).find {_ == 2} must beSome[Int] [alias: beSomething when type is not important]. " +
    "A which clause can be added to check additional properties: List('name').find {_ == 'name'} must beSome[String].which {_.size == 4}"  in {
      List(2).find {_ == 2} must beSome[Int]
      List(2).find {_ == 2} must beSomething
      List("name").find {_ == "name"} must beSome[String].which {_.size == 4}
      assertion(List().find {_ == 2} must beSomething) must failWith("None is not Some(x)")
    }
  }
}
