package scala.specs.matcher
import scala.specs.specification
import scala.specs.integration._
import scala.specs.Sugar._

object patternMatchersSpecSuite extends JUnit3(patternMatchersSpec) 
object patternMatchersSpec extends MatchersSpecification {
  "Pattern matchers" should { usingBefore { () => clearExample }
    "provide a beLike matcher using pattern matching: (1, 2) must beLike {case (1, _) => ok} " +
    "[ok is syntactic sugar for true from the Sugar trait]" in {
      "a" must beLike {case "a" => ok}
      ("a", "b") must beLike {case ("a", _) => ok}
      ("a", "b", "c") must beLike {case ("a", _, _) => ok}
      assertion(("a", "b", "c") must beLike {case ("a", _) => ok}) must 
                       failWith ("'(a,b,c)' doesn't match the expected pattern")
      assertion(("a", "b", "c") must not(beLike {case ("a", _, _) => ok})) must 
           failWith ("'(a,b,c)' matches the given pattern")
    }
    "provide a beNone matcher for options: List().find {_ == 2} must beNone" in {
      List().find {_ == 2} must beNone
      assertion(List(2).find {_ == 2} must beNone[Int]) must failWith("'Some(2)' is not None")
    }
    "provide a beSome matcher for options: List(2).find {_ == 2} must beSome[Int] [alias: beSomething when type is not important]" in {
      List(2).find {_ == 2} must beSome[Int]
      List(2).find {_ == 2} must beSomething
      assertion(List().find {_ == 2} must beSomething) must failWith("'None' is not Some(x)")
    }
  }
  "Pattern matchers" can { usingBefore { () => clearExample }
    "specify a which clause to check additional properties: List('name').find {_ == 'name'} must beSome[String].which {_.size == 4}" in {
      List("name").find {_ == "name"} must beSome[String].which {_.size == 4}
    } 
  }
}
