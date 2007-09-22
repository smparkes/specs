package scala.specs.matcher

import scala.specs._
import scala.specs.specification._

object Matchers extends Matchers
/**
 * <p>The <code>Matchers</code> trait provides all existing Matchers to the 
 * <code>Specification</code> trait</p> 
 */
trait Matchers extends AnyMatchers with 
                       LogicalMatchers with
                       StringMatchers with
                       IterableMatchers with
                       MapMatchers with
                       NumericMatchers with
                       ScalacheckMatchers with
                       PatternMatchers with 
                       MockMatchers with 
                       MatcherResult
                       
/**
 * <p>The <code>AbstractMatcher</code> class is used by the <code>Spec.must</code> method.
 * This class should be subclassed to provide an appropriate <code>apply</code>
 * method that will check a value <code>a</code></p> 
 * @param a a value to check
 */
abstract class AbstractMatcher[T] {
  def apply(a: => T): (Boolean, String, String)
}

/**
 * <p>This class is the base class for asserting if a given value, of type T, is matching an expected value</p>
 * <p>The <code>matcher</code> parameter is a function which takes a value and return a Boolean saying if the match is ok and
 * 2 messages: 1 if the match is ok, and another one if the match is ko. Those messages should usually specify which was the 
 * expected value and which was the actual one</p>
 * <p> It is also possible to use the boolean logical operator on matchers: and, or, not, xor to combine matchers together. 
 * This is the essential reason why the ok message is included in the <code>matcher</code> function. For instance, when the 
 * <code>not</code> operator is used, the ok message is used as a ko message</p>
 *   
 */
abstract class Matcher[T] extends AbstractMatcher[T] with MatcherResult {
  
  /**
   *  The <code>and</code> operator allow to combine to matchers through a logical and.
   *  <code>m1 and m2</code> can successfully match a value <code>a</code> only if m1 succeeds 
   *  and m2 succeeds also
   */   
  def and(m: => Matcher[T]): Matcher[T] = { 
    val outer = this
    new Matcher[T](){
    def apply(a: => T) = {
      val r1 = outer(a)
      if (!r1.success)
        (false, r1.okMessage, r1.koMessage)
      else {
        val r2 = m(a) 
        (r2.success, r1.okMessage + " and " + r2.okMessage, r1.okMessage + " but " + r2.koMessage) 
      }
  }}}

  /**
   *  The <code>or</code> operator allow to combine to matchers through a logical or.
   *  <code>m1 or m2</code> can successfully match a value <code>a</code> if m1 succeeds 
   *  or m2 succeeds
   */   
  def or(m: => Matcher[T]) : Matcher[T] = { 
    val outer = this
    new Matcher[T]() {
    def apply(a: =>T) = {
    val r1 = outer(a)
    val r2 = m(a) 
      if (!r1.success && !r2.success)
        (false, r1.okMessage + " and " + r2.okMessage, r1.koMessage + " and " + r2.koMessage) 
      else if (r1.success && !r2.success)
        (true, r1.okMessage + " but " + r2.koMessage, r1.koMessage + " and " + r2.koMessage)
      else if (!r1.success && r2.success)
        (true, r2.okMessage + " but " + r1.koMessage, r1.koMessage + " and " + r2.koMessage)
      else
        (true, r1.okMessage + " and " + r2.okMessage, r1.koMessage + " and " + r2.koMessage) 
  }}}

  /**
   *  The <code>xor</code> operator allow to combine to matchers through a logical xor.
   *  <code>m1 xor m2</code> can successfully match a value <code>a</code> if m1 succeeds 
   *  and m2 fails, or if m1 fails and m2 succeeds
   */   
  def xor(m: Matcher[T]) : Matcher[T] = (this and m.not) or (this.not and m)

  /**
   *  The <code>not</code> operator allow to combine to matchers through a logical not.
   *  <code>m1.not</code> returns a matcher failing if m1 succeeds, and succeeding if m1 fails
   */   
  def not = { 
    val outer = this;
    new Matcher[T]() {
    def apply(a: => T) = {
      val result = outer(a)
      (!result.success, result.koMessage, result.okMessage)
    }}
  }

  /**
   *  The <code>when</code> operator returns a matcher which will be ok only if a condition is true
   */   
  def when(condition : => Boolean) = { 
    val outer = this;
    new Matcher[T]() {
     def apply(a: => T) = {
          val result = outer(a)
          (if (condition) result.success else true, result.okMessage, result.koMessage)
      }}
  }
   /**
    *  The <code>unless</code> operator returns a matcher which will be ok only if a condition is false
    */   
   def unless(condition : => Boolean) = when(!condition) 
}
/**
 *  Result of <code>Matcher.apply</code>. Provides a success flag and status messages
 */   
trait MatcherResult {
  /**
   * This case class and the associated implicit definition is used to add more meaningful names to
   * the tuple representing the result of a match when implementing <code>Matcher</code> logical operators<br>
   * Usage: <code>matcher.apply(value).okMessage</code> for instance
   */  
  case class MatcherResult(success: Boolean, okMessage: String, koMessage: String)
  implicit def toMatcherResult(t: (Boolean, String, String)): MatcherResult = MatcherResult(t._1, t._2, t._3)  
}