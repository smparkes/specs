package scala.specs.matchers
import scala.specs.integration._

object logicalMatchersSuite extends JUnit3TestSuite(logicalMatchers) 
object logicalMatchers extends MatchersSpecification {
  "Logical matchers" should { usingBefore { () => clearExample }
    "provide a 'must not + matcher' matcher: 'name' must not(beMatching('abc'))" in {
      "name" must not(beMatching("abc"))
      assertion("name" must not(beMatching("n"))) must failWith("'name' matches 'n'")
    }
    "provide an 'and' matcher to combine matchers with a logical and. 'and' evaluation is stopped if the first argument is false" +
    "'string' must (beMatching('s') and beMatching('g'))" in {
      "string" must (beMatching("s") and beMatching("g"))
      assertion("string" must (beMatching("z") and beMatching("f"))) must 
                failWith("'string' doesn't match 'z'")
      assertion("string" must (beMatching("s") and beMatching("f"))) must 
                failWith("'string' matches 's' but 'string' doesn't match 'f'")
    }
    "provide an 'or' matcher to combine matchers with a logical or. 'or' evaluation is done for each operand " + 
    "'string' must (beMatching('s') or beMatching('g'))"  in {
      "string" must (beMatching("s") or beMatching("g"))
      "string" must (beMatching("s") or beMatching("z"))
      "string" must (beMatching("z") or beMatching("s"))
      assertion("string" must (beMatching("z") or beMatching("f"))) must 
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
    }
    "provide a 'verifyAll' matcher which is ok if every matcher is ok"  in {
        "string" must verifyAll (beMatching("s"), beMatching("g"))
        assertion("string" must verifyAll (beMatching("s"), beMatching("f"))) must 
                  failWith("'string' matches 's' but 'string' doesn't match 'f'")
    }
    "provide a 'verifyAny' matcher which is ok if at least one matcher is ok"  in {
      "string" must verifyAny (beMatching("s"), beMatching("z"))
      assertion("string" must verifyAny (beMatching("z"), beMatching("f"))) must 
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
    }
  }
}
object numericTestSuite extends JUnit3TestSuite(numericMatchers) 
object numericMatchers extends MatchersSpecification {
  "Numeric matchers" should { usingBefore { () => clearExample }
    "provide a 'must beClose' matcher: 1.2 must beClose(1.0, 0.5)" in {
      1.2 must beClose(1.0, 0.5)
      1 must beClose(1, 1)
      assertion(1.2 must beClose(1.0, 0.1)) must failWith("1.2 is not close 1.0 +/- 0.1")
      assertion(1.2 must not(beClose(1.0, 0.2))) must failWith("1.2 is close 1.0 +/- 0.2")
    }
  }
}
