package scala.specs.specification
import scala.specs.matcher._
import scala.specs.matcher.Matchers._

/**
 * The assert class adds matcher methods to objects which are being specified
 * Usage: <code>new Assert(value, example) must 'matcher'(otherValue)</code>
 *
 * An assert is created with its parent <code>Example</code> in order to register failures
 * and errors if a matcher is not ok 
 *
 */
class Assert[+T](value: => T, example: Example) {
  /** increment the number of assertions of the example when this object is created */
  example.assertionsNb += 1
  
  /**
   * applies a matcher to the current value and throw a failure is the result is not true 
   */
  def must[S >: T](m: => Matcher[S]): Boolean =  {
    val (result, _, koMessage) = m.apply(value) 
    result match {
      case false => throwFailure(this, koMessage)
      case _ => true
    }
  }

  /** alias for <code>must verify(f)</code>  */
  def mustVerify[S >: T](f: S => Boolean): Boolean = must[S](verify(f))

  /** alias for <code>mustVerify(f)</code>  */
  def verifies(f: T => Boolean) = mustVerify(f)

  /** alias for <code>must be(other)</code>  */
  def mustBe(otherValue: Any) = must(be(otherValue)) 

  /** alias for <code>must be(other)</code>  */
  def mustEq(otherValue: Any) = must(be(otherValue))

  /** alias for <code>must notEq(other)</code>  */
  def mustNotBe(otherValue: Any) = must(notEq(otherValue)) 

  /** alias for <code>must notEq(other)</code>  */
  def mustNotEq(otherValue: Any) = mustNotBe(otherValue)

  /** alias for <code>must is_!=(other)</code>  */
  def must_!=(otherValue: Any) = must(is_!=(otherValue))

  /** alias for <code>must is_==(other)</code>  */
  def must_==(otherValue: Any) = must(is_==(otherValue))

  /** alias for <code>must is_==(other)</code>  */
  def mustEqual(otherValue: Any) = must(is_==(otherValue))
}
/** RuntimeException carrying a matcher ko message */
case class FailureException(message: String) extends RuntimeException(message)

/** Specialized assert class with string matchers aliases */
class AssertString[A <: String](value: => A, example: Example) extends Assert[A](value, example) {
  def mustMatch(a: String) = must(beMatching(a))
  def mustNotMatch(a: String) = must(not(beMatching(a)))
  def must_==/(a: String) = must(equalIgnoreCase(a))
  def must_!=/(a: String) = must(notEqualIgnoreCase(a))
}
/** Specialized assert class with iterable matchers aliases */
class AssertIterable[I <: AnyRef](value: Iterable[I], example: Example) extends Assert[Iterable[I]](value, example) {
  def mustExist(function: I => Boolean) = must(exist {x:I => function(x)})
  def mustNotExist(function: I => Boolean) = must(notExist{x:I => function(x)})
  def mustContain(elem: I) = must(contain(elem))
  def mustNotContain(elem: I) = must(notContain(elem))
}
/** Specialized assert class with iterable[String] matchers aliases */
class AssertIterableString(value: Iterable[String], example: Example) extends AssertIterable[String](value, example) {
  def mustHaveMatch(elem: String) = must(existMatch(elem))
  def mustNotHaveMatch(elem: String) = must(notExistMatch(elem))
  def mustExistMatch(pattern: String) = must(existMatch(pattern))
  def mustNotExistMatch(pattern: String) = must(notExistMatch(pattern))
}
