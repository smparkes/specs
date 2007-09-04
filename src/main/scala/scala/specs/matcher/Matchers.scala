package scala.specs.matcher

import scala.specs._
import scala.specs.specification._
import scala.specs.matcher.Matcher._

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
                       ScalaCheckMatchers with
                       PatternMatchers with 
                       MockMatchers
                       
/**
 * <p>The <code>AbstractMatcher</code> class is used by the Spec.must method.
 * This class can be subclassed and provide an appropriate <code>apply</code>
 * method that will check a value <code>a</code></p> 
 */
abstract class AbstractMatcher[T] {
  def apply(a: T): (Boolean, String, String)
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
class Matcher[T](val matcher: T => (Boolean, String, String)) extends AbstractMatcher[T] {
   /**
    *  Applying the matcher means executing the embedded <code>matcher</code> function to yield a result
    */ 
   def apply(a: T) = matcher(a) 

  /**
   * This case class and the associated implicit definition is only here to add more meaningful names to
   * the tuple components in the following operators 
   */  
  case class MatcherResult(success: Boolean, okMessage: String, koMessage: String)
  implicit def toMatcherResult(t: (Boolean, String, String)): MatcherResult = MatcherResult(t._1, t._2, t._3)  
  
  /**
   *  The <code>and</code> operator allow to combine to matchers through a logical and.
   *  <code>m1 and m2</code> can successfully match a value <code>a</code> only if m1 succeeds 
   *  and m2 succeeds also
   */   
  def and(m: Matcher[T]): Matcher[T] = make[T]((a: T) => {
      val r1 = this(a)
      if (!r1.success)
        (false, r1.okMessage, r1.koMessage)
      else {
        val r2 = m(a) 
        (r2.success, r1.okMessage + " and " + r2.okMessage, r1.okMessage + " but " + r2.koMessage) 
      }
  })

  /**
   *  The <code>or</code> operator allow to combine to matchers through a logical or.
   *  <code>m1 or m2</code> can successfully match a value <code>a</code> if m1 succeeds 
   *  or m2 succeeds
   */   
  def or(m: Matcher[T]) : Matcher[T] = make[T]((a: T) => {
    val r1 = this(a)
    val r2 = m(a) 
      if (!r1.success)
        (r2.success, r2.okMessage, r1.koMessage + " and " + r2.koMessage) 
      else if (!r2.success)
        (r1.success, r1.okMessage, r1.koMessage + " and " + r2.koMessage)
      else
        (r1.success || r2.success, r1.okMessage + " and " + r2.okMessage, r1.koMessage + " and " + r2.koMessage) 
  })

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
  def not = make[T]((a: T) => {
    val result = matcher(a)
    (!result.success, result.koMessage, result.okMessage)
  })
  override def toString = matcher.toString
}

/**
 * The <code>Matcher</code> object provides a factory method to create Matcher objects
 */  
object Matcher {
  def make[T](m: T => (Boolean, String, String)) = new Matcher[T](m)
}
