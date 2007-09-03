package scala.specs.matcher

import scala.specs._
import scala.specs.specification._
import scala.specs.matcher.Matcher._

object Matchers extends Matchers
trait Matchers extends AnyMatchers with 
                       LogicalMatchers with
                       StringMatchers with
                       IterableMatchers with
                       MapMatchers with
                       NumericMatchers with
                       ScalaCheckMatchers with
                       PatternMatchers with 
                       MockMatchers
                       
abstract class AbstractMatcher[T] {
  def apply(a: T): (Boolean, String, String)
  def and(m: Matcher[T]): Matcher[T]
  def or(m: Matcher[T]): Matcher[T]
  def not: Matcher[T]
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
  def apply(a: T) = matcher(a) 
  def and(m: Matcher[T]): Matcher[T] = make[T]((a: T) => {
      val result1 = this(a)
      if (!result1._1)
        (false, result1._2, result1._3)
      else {
        val result2 = m(a) 
        (result2._1, result1._2 + " and " + result2._2, result1._2 + " but " + result2._3) 
      }
  })
  def or(m: Matcher[T]) : Matcher[T] = make[T]((a: T) => {
    val result1 = this(a)
    val result2 = m(a) 
      if (!result1._1)
        (result2._1, result2._2, result1._3 + " and " + result2._3) 
      else if (!result2._1)
        (result1._1, result1._2, result1._3 + " and " + result2._3)
      else
        (result1._1 || result2._1, result1._2 + " and " + result2._2, result1._3 + " and " + result2._3) 
  })
  def xor(m: Matcher[T]) : Matcher[T] = (this and m.not) or (this.not and m)
  def not = make[T]((a: T) => {
    val result = matcher(a)
    (!result._1, result._3, result._2)
  })
  override def toString = matcher.toString
}
object Matcher {
  def make[T](m: T => (Boolean, String, String)) = new Matcher[T](m)
}
