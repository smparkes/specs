package scala.specs
import scala.collection.mutable.Queue
import scala.specs.integration._

object assertTestSuite extends JUnit3TestSuite(assertSpec) 
object assertSpec extends Specification with ExampleReporter with Sugar {

  "An 'Assert' object" should { usingBefore { () => clearExample }
    "have a 'must_==' matcher: 'name' must_== 'name'" in {
      "string" must_== "string"
      assertion("string" must_== "string2") must failWith("'string' is not equal to 'string2'")  
    }
    "have a 'must_!=' matcher 'name' must_!= 'name2'" in {
      "string" must_!= "string2"
      assertion("string" must_!= "string") must failWith("'string' is equal to 'string'")
    }
    "have a 'must_==/' matcher: 'hello' must_==/ 'HeLLo' " + 
    "[alias: must equalsIgnoreCase]" in {
      "string" must_==/ "sTring"
      "string" must equalIgnoreCase("sTring")
      assertion("string" must_==/ "striNg2") must failWith("'string' is not equal ignoring case to 'striNg2'")  
    }
    "have a 'must_!=/' matcher: 'name' must_!=/ 'naME' will fail" + 
    "[alias: notEqualIgnoreCase]" in {
      "string" must_!=/ "sTring2"
      "string" must notEqualIgnoreCase("sTring2")
      assertion("string" must_!=/ "strinG") must failWith("'string' is equal ignoring case to 'strinG'")  
    }
    "have a 'must be' matcher: o1 must be(o2) if they are the same object " + 
    "('must eq' cannot be used because it overrides the eq matcher from Object) [alias: mustBe, mustEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1
        o1 must be(o2)
        o1 mustBe o2
        o1 mustEq o2
        assertion(o1 must be(o3)) must failWith("'o1' is not the same as 'o3'")
    }
    "have a 'must notBe' matcher: o1 must notBe(o2) if they are not the same object [alias: mustNotBe, mustNotEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1

        o1 must notBe(o3)
        o1 mustNotBe o3
        o1 mustNotEq o3
        assertion(o1 must notBe(o2)) must failWith("'o1' is the same as 'o1'")
    }
    "have a 'verifies' matcher checking if an object verifies a property: 'name' verifies {_.size == 4} [alias: verify]" in {
      List() verifies { _.isEmpty }
      List() verify { _.isEmpty }
      assertion(List("1") verifies { _.isEmpty }) must failWith("List(1) doesn't verify the expected property")
    }
    "have a 'must beMatching' matcher to match a pattern inside a string: " +
    " 'name' must beMatching('.*am.*') [alias: mustMatch]" in {
      "name" must beMatching(".*am.*")
      "name" mustMatch "name"
      assertion("name" must beMatching("xxx")) must failWith("'name' doesn't match 'xxx'")
    }
    "have a 'must notBeMatching' matcher not to match a pattern inside a string [alias: mustNotMatch]" in {
      "name" must notBeMatching("abc")
      "name" mustNotMatch "abc"
      assertion("name" must notBeMatching("n")) must failWith("'name' matches 'n'")
    }
    "have a 'must include' matcher: 'name' must include('am')" in {
      "name" must include("am")
      assertion("name" must include("oo")) must failWith("'name' doesn't include 'oo'")
    }
    "have a 'must notInclude' matcher: 'name' must notInclude('oo')" in {
      "name" must notInclude("oo")
      assertion("name" must notInclude("am")) must failWith("'name' includes 'am'")
    }
    "have a 'must startWith' matcher: 'name' must startWith('na')" in {
      "name" must startWith("na")
      assertion("name" must startWith("oo")) must failWith("'name' doesn't start with 'oo'")
    }
    "have a 'must notStartWith' matcher: 'name' must notStartWith('am')" in {
      "name" must notStartWith("oo")
      assertion("name" must notStartWith("na")) must failWith("'name' starts with 'na'")
    }
    "have a 'must endWith' matcher: 'name' must endWith('me')" in {
      "name" must endWith("me")
      assertion("name" must endWith("oo")) must failWith("'name' doesn't end with 'oo'")
    }
    "have a 'must notEndWith' matcher: 'name' must notEndWith('oo')" in {
      "name" must notEndWith("oo")
      assertion("name" must notEndWith("me")) must failWith("'name' ends with 'me'")
    }
    "have a 'must beEmpty' matcher on iterables: List() must beEmpty" in {
      List() must beEmpty
      assertion(List("1") must beEmpty) must failWith("List(1) is not empty")
    }
    "have a 'must notBeEmpty' matcher on iterables: List(1) must notBeEmpty" in {
      List("1") must notBeEmpty
      assertion(List() must notBeEmpty) must failWith("List() is empty")
    }
    "have a 'must contain' matcher on iterables: List(1) must contain(1) [alias: mustContain]" in {
      List("one", "two") must contain("one")
      assertion(List("one", "two") mustContain "three") must failWith("List(one, two) doesn't contain 'three'")
    }
    "have a 'must notContain' matcher on iterables: List(1) must notContain(2) [alias: mustNotContain]" in {
      List("one", "two") must notContain("three")
      assertion(List("one", "two") mustNotContain "one") must failWith("List(one, two) contains 'one'")
    }
    "have a 'must beIn' matcher on iterables: 'one' must beIn(List('one', 'two'))" in {
      "one" must beIn(List("one", "two"))
      assertion("three" must beIn(List("one", "two"))) must failWith("'three' is not in List(one, two)")
    }
    "have a 'must notBeIn' matcher on iterables: 'three' must notBeIn(List('one', 'two'))" in {
      "three" must notBeIn(List("one", "two"))
      assertion("one" must notBeIn(List("one", "two"))) must failWith("'one' is in List(one, two)")
    }
    "have a 'must exist' matcher on iterables: List('one', 'two') must exist {m: String => m.contains('w')} [alias: mustExist]" in {
      List("one", "two") must exist {m: String => m.contains("w")}
      assertion(List("one", "two") mustExist {m: String => m.contains("z")}) must failWith("no element verifies the property in List(one, two)")
    }
    "have a 'must notExist' matcher on iterables: List('one', 'two') must notExist {m: String => m.contains('z')}  [alias: mustNotExist]" in {
      List("one", "two") must notExist {m: String => m.contains("z")}
      assertion(List("one", "two") mustNotExist {m: String => m.contains("n")}) must failWith("at least one element verifies the property in List(one, two)")
    }
    "have a 'must existMatch' matcher on iterables: checking if it contains a string including a regexp: " +
    "List('one', 'two') must existMatch('[n-o]') [alias: mustExistMatch]" in {
      List("one", "two") must existMatch("[n-o]")
      assertion(List("one", "two") mustExistMatch("[a-c]")) must failWith("no element matches '[a-c]' in List(one, two)")
    }
    "have a 'must notExistMatch' matcher checking if it doesn't contain a string including a regexp: " +
    "List('one', 'two') must existMatch('[a-c]') [alias: mustNotExistMatch]" in {
      List("one", "two") must notExistMatch("[a-c]")
      assertion(List("one", "two") mustNotExistMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in List(one, two)")
    }
    "have a 'mustThrow' matcher expecting a block to send an exception of a given type" in {
      {throw new Error("user error"); ()} mustThrow new Error()

      class MyError(msg: String) extends Error(msg) {}
      {throw new MyError("subclass of error"); ()} mustThrow new Error() 

      assertion({throw new NullPointerException; ()} mustThrow new Error()) must failWith("java.lang.Error should have been thrown. Got: java.lang.NullPointerException")
    } 
    "have an 'haveKey' matcher on maps: Map('one' -> 1, 'two' -> 2) must haveKey('one') [alias for not + haveKey = notHaveKey]" in {
      Map("one" -> 1, "two" -> 2) must haveKey("one")
      assertion(Map("one" -> 1, "two" -> 2) must haveKey("three")) must failWith("Map(one -> 1, two -> 2) hasn't key 'three'")
      assertion(Map("one" -> 1, "two" -> 2) must notHaveKey("one")) must failWith("Map(one -> 1, two -> 2) has key 'one'")
    }
    "have an 'haveValue' matcher on maps: Map('one' -> 1, 'two' -> 2) must haveValue(1) [alias for not + haveValue = notHaveValue]" in {
      Map("one" -> 1, "two" -> 2) must haveValue(1)
      assertion(Map("one" -> 1, "two" -> 2) must haveValue(3)) must failWith("Map(one -> 1, two -> 2) hasn't value '3'")
      assertion(Map("one" -> 1, "two" -> 2) must notHaveValue(1)) must failWith("Map(one -> 1, two -> 2) has value '1'")
    }
    "have an 'havePair' matcher on maps: Map('one' -> 1, 'two' -> 2) must havePair('one' -> 1) [alias for not + havePair = notHavePair]" in {
      Map("one" -> 1, "two" -> 2) must havePair("one" -> 1)
      assertion(Map("one" -> 1, "two" -> 2) must havePair("one" -> 3)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(one,3)'")
      assertion(Map("one" -> 1, "two" -> 2) must havePair("three" -> 1)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(three,1)'")
      assertion(Map("one" -> 1, "two" -> 2) must havePair("three" -> 3)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(three,3)'")
      assertion(Map("one" -> 1, "two" -> 2) must not(havePair("one" -> 1))) must failWith("Map(one -> 1, two -> 2) has pair '(one,1)'")
      assertion(Map("one" -> 1, "two" -> 2) must notHavePair("one" -> 1)) must failWith("Map(one -> 1, two -> 2) has pair '(one,1)'")
    }
    "have a 'must beClose' matcher: 1.2 must beClose(1.0, 0.5)" in {
      1.2 must beClose(1.0, 0.5)
      1 must beClose(1, 1)
      assertion(1.2 must beClose(1.0, 0.1)) must failWith("1.2 is not close 1.0 +/- 0.1")
      assertion(1.2 must not(beClose(1.0, 0.2))) must failWith("1.2 is close 1.0 +/- 0.2")
    }
    "have a 'must not + matcher' matcher: 'name' must not(beMatching('abc'))" in {
      "name" must not(beMatching("abc"))
      assertion("name" must not(beMatching("n"))) must failWith("'name' matches 'n'")
    }
    "have an 'and' matcher to combine matchers with a logical and. 'and' evaluation is stopped if the first argument is false" +
    "'string' must (beMatching('s') and beMatching('g'))" in {
      "string" must (beMatching("s") and beMatching("g"))
      assertion("string" must (beMatching("z") and beMatching("f"))) must 
                failWith("'string' doesn't match 'z'")
      assertion("string" must (beMatching("s") and beMatching("f"))) must 
                failWith("'string' matches 's' but 'string' doesn't match 'f'")
    }
    "have an 'or' matcher to combine matchers with a logical or. 'or' evaluation is done for each operand " + 
    "'string' must (beMatching('s') or beMatching('g'))"  in {
      "string" must (beMatching("s") or beMatching("g"))
      "string" must (beMatching("s") or beMatching("z"))
      "string" must (beMatching("z") or beMatching("s"))
      assertion("string" must (beMatching("z") or beMatching("f"))) must 
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
    }
    "have a beLike matcher using pattern matching: (1, 2) must beLike {case (1, _) => ok} " +
    "[ok is syntactic sugar for true from the Sugar trait]" in {
      "a" must beLike {case "a" => ok}
      ("a", "b") must beLike {case ("a", _) => ok}
      ("a", "b", "c") must beLike {case ("a", _, _) => ok}
      assertion(("a", "b", "c") must beLike {case ("a", _) => ok}) must 
                       failWith ("(a,b,c) doesn't match the expected pattern")
      assertion(("a", "b", "c") must not(beLike {case ("a", _, _) => ok})) must 
           failWith ("(a,b,c) matches the given pattern")
    }
    "have a beNone matcher for options: List().find {_ == 2} must beNone" in {
      List().find {_ == 2} must beNone
      assertion(List(2).find {_ == 2} must beNone[Int]) must failWith("Some(2) is not None")
    }
    "have a beSome matcher for options: List(2).find {_ == 2} must beSome[Int] [alias: beSomething when type is not important]. " +
    "A which clause can be added to check additional properties: List('name').find {_ == 'name'} must beSome[String].which {_.size == 4}"  in {
      List(2).find {_ == 2} must beSome[Int]
      List(2).find {_ == 2} must beSomething
      List("name").find {_ == "name"} must beSome[String].which {_.size == 4}
      assertion(List().find {_ == 2} must beSomething) must failWith("None is not Some(x)")
    }
  }
  def failWith(message: String) = is_==(message)
  def assertion(value: => Any): String = {
    try {
      value
    } catch {
      case FailureException(message) => return message
      case t: Throwable => throw t
    }
    return "this assertion has not failed"
  }
}
trait ExampleReporter {
  var reported: Example = new Example("this example serves as a stub to collect failure messages", new Sut(""))
  def clearExample = { reported = new Example("", new Sut("")) }
}
