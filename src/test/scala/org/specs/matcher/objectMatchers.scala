package scala.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._

object objectMatchersTestSuite extends JUnit3(objectMatchersSpec) 
object objectMatchersSpec extends MatchersSpecification {
  "Object matchers" should { usingBefore { () => clearExample }
    "provide a 'must_==' matcher: 'name' must_== 'name'" in {
      "string" must_== "string"
      assertion("string" must_== "string2") must failWith("'string' is not equal to 'string2'")  
    }
    "provide a 'must_!=' matcher 'name' must_!= 'name2'" in {
      "string" must_!= "string2"
      assertion("string" must_!= "string") must failWith("'string' is equal to 'string'")
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
      assertion(List("1") verifies { _.isEmpty }) must failWith("List(1) doesn't verify the expected property")
    }

    "provide a 'mustThrow' matcher expecting a block to send an exception of a given type" in {
      {throw new Error("user error"); ()} must throwA(new Error)

      class MyError(msg: String) extends Error(msg) {}
      {throw new MyError("subclass of error"); ()} must throwA(new Error) 

      assertion({throw new NullPointerException; ()} must throwA(new Error)) must failWith("java.lang.Error should have been thrown. Got: java.lang.NullPointerException")
    }
  }   
}
