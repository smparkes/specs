package org.specs.matcher
import org.specs.runner._

class mapMatchersTest extends JUnit3(mapMatchersUnit)
object mapMatchersUnit extends MatchersSpecification {
  "Map matchers" should {
    "not evaluate the expressions twice: haveKey" in {
      val map: Iterable[(String, Any)] = Map("" -> 1)
      haveKey("") must evalOnce(exp(map))
    }
    "not evaluate the expressions twice: haveValue" in {
      val map2: Iterable[(Any, Int)] = Map("" -> 1)
      haveValue(1) must evalOnce(exp(map2))
    }
    "not evaluate the expressions twice: havePair" in {
      val map3: Iterable[(String, Int)] = Map("" -> 1)
      havePair("" ->1) must evalOnce(exp(map3))
    }
  }
}
