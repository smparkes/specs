package scala.specs.matcher
import scala.specs.runner._

object mapMatchersSpecSuite extends JUnit3(mapMatchersSpec) 
object mapMatchersSpec extends MatchersSpecification {
  "Map matchers" should { usingBefore { () => clearExample }
    "provide an 'haveKey' matcher on maps: Map('one' -> 1, 'two' -> 2) must haveKey('one') [alias for not + haveKey = notHaveKey]" in {
      Map("one" -> 1, "two" -> 2) must haveKey("one")
      assertion(Map("one" -> 1, "two" -> 2) must haveKey("three")) must failWith("Map(one -> 1, two -> 2) hasn't key 'three'")
      assertion(Map("one" -> 1, "two" -> 2) must notHaveKey("one")) must failWith("Map(one -> 1, two -> 2) has key 'one'")
    }
    "provide an 'haveValue' matcher on maps: Map('one' -> 1, 'two' -> 2) must haveValue(1) [alias for not + haveValue = notHaveValue]" in {
      Map("one" -> 1, "two" -> 2) must haveValue(1)
      assertion(Map("one" -> 1, "two" -> 2) must haveValue(3)) must failWith("Map(one -> 1, two -> 2) hasn't value '3'")
      assertion(Map("one" -> 1, "two" -> 2) must notHaveValue(1)) must failWith("Map(one -> 1, two -> 2) has value '1'")
    }
    "provide an 'havePair' matcher on maps: Map('one' -> 1, 'two' -> 2) must havePair('one' -> 1) [alias for not + havePair = notHavePair]" in {
      Map("one" -> 1, "two" -> 2) must havePair("one" -> 1)
      assertion(Map("one" -> 1, "two" -> 2) must havePair("one" -> 3)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(one,3)'")
      assertion(Map("one" -> 1, "two" -> 2) must havePair("three" -> 1)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(three,1)'")
      assertion(Map("one" -> 1, "two" -> 2) must havePair("three" -> 3)) must failWith("Map(one -> 1, two -> 2) hasn't pair '(three,3)'")
      assertion(Map("one" -> 1, "two" -> 2) must not(havePair("one" -> 1))) must failWith("Map(one -> 1, two -> 2) has pair '(one,1)'")
      assertion(Map("one" -> 1, "two" -> 2) must notHavePair("one" -> 1)) must failWith("Map(one -> 1, two -> 2) has pair '(one,1)'")
    }
  }
}
