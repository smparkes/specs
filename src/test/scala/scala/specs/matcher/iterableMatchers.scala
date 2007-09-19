package scala.specs.matcher
import scala.specs.runner._
import scala.specs._

object iterableTestSuite extends JUnit3(iterableMatchersSpec) 
object iterableMatchersSpec extends MatchersSpecification {
  "Iterable matchers" should { usingBefore { () => clearExample }
    "provide a 'must beEmpty' matcher on iterables: List() must beEmpty" in {
      List() must beEmpty
      assertion(List("1") must beEmpty) must failWith("List(1) is not empty")
    }
    "provide a 'must notBeEmpty' matcher on iterables: List(1) must notBeEmpty" in {
      List("1") must notBeEmpty
      assertion(List() must notBeEmpty) must failWith("List() is empty")
    }
    "provide a 'must contain' matcher on iterables: List(1) must contain(1) [alias: mustContain]" in {
      List("one", "two") must contain("one")
      assertion(List("one", "two") mustContain "three") must failWith("'List(one, two)' doesn't contain 'three'")
    }
    "provide a 'must notContain' matcher on iterables: List(1) must notContain(2) [alias: mustNotContain]" in {
      List("one", "two") must notContain("three")
      assertion(List("one", "two") mustNotContain "one") must failWith("'List(one, two)' contains 'one'")
    }
    "provide a 'must beIn' matcher on iterables: 'one' must beIn(List('one', 'two'))" in {
      "one" must beIn(List("one", "two"))
      assertion("three" must beIn(List("one", "two"))) must failWith("'three' is not in 'List(one, two)'")
    }
    "provide a 'must notBeIn' matcher on iterables: 'three' must notBeIn(List('one', 'two'))" in {
      "three" must notBeIn(List("one", "two"))
      assertion("one" must notBeIn(List("one", "two"))) must failWith("'one' is in 'List(one, two)'")
    }
    "provide a 'must exist' matcher on iterables: List('one', 'two') must exist {m: String => m.contains('w')} [alias: mustExist]" in {
      List("one", "two") must exist {m: String => m.contains("w")}
      assertion(List("one", "two") mustExist {m: String => m.contains("z")}) must failWith("no element verifies the property in 'List(one, two)'")
    }
    "provide a 'must notExist' matcher on iterables: List('one', 'two') must notExist {m: String => m.contains('z')}  [alias: mustNotExist]" in {
      List("one", "two") must notExist {m: String => m.contains("z")}
      assertion(List("one", "two") mustNotExist {m: String => m.contains("n")}) must failWith("at least one element verifies the property in 'List(one, two)'")
    }
    "provide a 'must existMatch' matcher on iterables: checking if it contains a string including a regexp: " +
    "List('one', 'two') must existMatch('[n-o]') [alias: mustExistMatch]" in {
      List("one", "two") must existMatch("[n-o]")
      assertion(List("one", "two") mustExistMatch("[a-c]")) must failWith("no element matches '[a-c]' in 'List(one, two)'")
    }
    "provide a 'must notExistMatch' matcher checking if it doesn't contain a string including a regexp: " +
    "List('one', 'two') must existMatch('[a-c]') [alias: mustNotExistMatch]" in {
      List("one", "two") must notExistMatch("[a-c]")
      assertion(List("one", "two") mustNotExistMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in 'List(one, two)'")
    }
  }
}
