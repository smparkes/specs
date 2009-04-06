package org.specs.matcher
import org.specs.specification._
import org.specs.execute._

class anyMatcherSpec extends MatchersSpecification {
  "A matcher" can {
    "be created as a case class" in {
      case class matchHello(a: String) extends Matcher[String]() {
        def apply(v: => String) = (v == a, "okMessage", "koMessage")
      }
      "hello" must matchHello("hello")
    }
    "be created as a val" in {
      val beEven = new Matcher[Int] {
        def apply(b: => Int) = (b % 2 == 0, b + " is even", b + " is odd")
      }
      2 must beEven
    }
    "be created as a method" in {
      def divide(a: Int) = new Matcher[Int] {
        def apply(b: => Int) = (a % b == 0, b + " divides " + a, b + " doesn't divide " + a)
      }
      10 must divide(100)
      3 must not(divide(100))
    }
    "cause the example to be skipped" in {
      expectation( 1 must be_==(2).orSkipExample ) must throwThis(SkippedException("skipped because '1' is not equal to '2'"))
    }
    "match lazy values" in {
      val lazyVal = () => 1
      lazyVal must be_==(1).lazily
    }
    "be composed with a function using the ^^ operator and return a matcher" in {
      6 must beEqualTo("66") ^^ ((_:Int).toString * 2)
    }
    "be composed with a function using the ^^ operator and return a matcher" in {
      6 must ((beEqualTo(_:Int)) ^^ ((_:String).size)) ("string")
      // equivalent to
      6 must beEqualTo(((_:String).size)("string"))
    }
    "be composed with a function using the ^^^ operator and return a function returning a matcher" in {
      "123456" must ((beEqualTo(_:Int)) ^^^ ((_: String).size))("string")
      // equivalent to
      ((_:String).size)("123456") must beEqualTo(((_:String).size)("string"))
    }
    "transformed to a matcher matching a sequence of objects using the toSeq method" in {
      List("a", "b", "c") must (beEqualTo(_:String)).toSeq(List("a", "b", "c"))
      expectation(List("a", "c", "b") must (beEqualTo(_:String)).toSeq(List("a", "b", "c"))) must
        failWith("'c' is not equal to 'b'; 'b' is not equal to 'c'")
    }
    "transformed to a matcher matching a set of objects using the toSet method" in {
      Set("a", "b", "c") must (beEqualTo(_:String)).toSet(Set("b", "a", "c"))
      expectation(Set("a", "b", "c") must (beEqualTo(_:String)).toSet(Set("b", "a", "d"))) must
        failWith("no match for element 'c'")
    }
    "provide a toSeq method which can be composed with a function" in {
      List(3, 1, 2) must ((beEqualTo(_:Int)) ^^ ((_:String).size)).toSeq(List("abc", "a", "ab"))
    }
    "be equal with is_== even if using an alias for the object" in {
       "this object" aka "this" must_== "this object"
    }
    "use the alias declaration for a string being tested with is_==" in {
       val s = "that" aka "that string"
       s must_== "that"

       "this" aka "this string" must_== "this"
       "this" aka "this string" must beMatching("t")
       expectation("hello" aka "the word" must_== "that") must failWith("the word 'hello' is not equal to 'that'")
    }
    "use the alias declaration for the object being tested with is_==" in {
       expectation(1 aka "this" must_== 2) must failWith("this '1' is not equal to '2'")
    }
    "use the alias declaration for a list being tested with is_==" in {
       List(1, 2) aka "the list" must haveSize(2)
       expectation(List(1, 2) aka "the list" must_== List(1, 4)) must failWith("the list 'List(1, 2)' is not equal to 'List(1, 4)'")
    }
    "use the alias declaration for a list of strings being tested with is_==" in {
       List("1", "2") aka "the list" must containMatch("\\d")
       expectation(List("2") aka "the list" must_== List("1")) must failWith("the list 'List(2)' is not equal to 'List(1)'")
    }
  }
}
