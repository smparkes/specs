package scala.specs.matcher
import scala.specs.integration._

object stringMatchersSpecSuite extends JUnit3(stringMatchersSpec) 
object stringMatchersSpec extends MatchersSpecification {
  "String matchers" should { usingBefore { () => clearExample }
    "provide a 'must_==/' matcher: 'hello' must_==/ 'HeLLo' " + 
    "[alias: must equalsIgnoreCase]" in {
      "string" must_==/ "sTring"
      "string" must equalIgnoreCase("sTring")
      assertion("string" must_==/ "striNg2") must failWith("'string' is not equal ignoring case to 'striNg2'")  
    }
    "provide a 'must_!=/' matcher: 'name' must_!=/ 'naME' will fail" + 
    "[alias: notEqualIgnoreCase]" in {
      "string" must_!=/ "sTring2"
      "string" must notEqualIgnoreCase("sTring2")
      assertion("string" must_!=/ "strinG") must failWith("'string' is equal ignoring case to 'strinG'")  
    }
    "provide a 'must beMatching' matcher to match a pattern inside a string: " +
    " 'name' must beMatching('.*am.*') [alias: mustMatch]" in {
      "name" must beMatching(".*am.*")
      "name" mustMatch "name"
      assertion("name" must beMatching("xxx")) must failWith("'name' doesn't match 'xxx'")
    }
    "provide a 'must notBeMatching' matcher not to match a pattern inside a string [alias: mustNotMatch]" in {
      "name" must notBeMatching("abc")
      "name" mustNotMatch "abc"
      assertion("name" must notBeMatching("n")) must failWith("'name' matches 'n'")
    }
    "provide a 'must include' matcher: 'name' must include('am')" in {
      "name" must include("am")
      assertion("name" must include("oo")) must failWith("'name' doesn't include 'oo'")
    }
    "provide a 'must notInclude' matcher: 'name' must notInclude('oo')" in {
      "name" must notInclude("oo")
      assertion("name" must notInclude("am")) must failWith("'name' includes 'am'")
    }
    "provide a 'must startWith' matcher: 'name' must startWith('na')" in {
      "name" must startWith("na")
      assertion("name" must startWith("oo")) must failWith("'name' doesn't start with 'oo'")
    }
    "provide a 'must notStartWith' matcher: 'name' must notStartWith('am')" in {
      "name" must notStartWith("oo")
      assertion("name" must notStartWith("na")) must failWith("'name' starts with 'na'")
    }
    "provide a 'must endWith' matcher: 'name' must endWith('me')" in {
      "name" must endWith("me")
      assertion("name" must endWith("oo")) must failWith("'name' doesn't end with 'oo'")
    }
    "provide a 'must notEndWith' matcher: 'name' must notEndWith('oo')" in {
      "name" must notEndWith("oo")
      assertion("name" must notEndWith("me")) must failWith("'name' ends with 'me'")
    }
  }
}
