package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>IterableMatchers</code> trait provides matchers which are applicable to Iterable objects
 */
trait IterableMatchers {
   
  /**
   * Matches if (iterable.exists(_ == a)
   */   
  def contain[T](a: T) = make[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists(_ == a), q(iterable) + " contains " + q(a), q(iterable) + " doesn't contain " + q(a))) 

  /**
   * Matches if not(iterable.exists(_ == a)
   */   
  def notContain[T](a: T) = contain(a).not 

  /**
   * Matches if there is one element in the iterable verifying the <code>function</code>: (iterable.exists(function(_))
   */   
  def exist[T](function: T => Boolean) = make[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists{function(_)}, "at least one element verifies the property in " + q(iterable), "no element verifies the property in " + q(iterable))) 

  /**
   * Matches if there is no element in the iterable verifying the <code>function</code>: !(iterable.exists(function(_))
   */   
  def notExist[T](function: T => Boolean) = exist(function).not 

  /**
   * Matches if there is one element in the iterable[String] matching the <code>pattern</code>: iterable.exists( matches(pattern) _)
   */   
  def existMatch(pattern: String) = make[Iterable[String]]((iterable: Iterable[String]) => (iterable.exists( matches(pattern) _), "at least one element matches " + q(pattern) + " in " + q(iterable), "no element matches " + q(pattern) + " in " + q(iterable))) 

  /**
   * Alias for existMatch
   */   
  def containMatch(pattern: String) = existMatch(pattern) 

  /**
   * Matches if not(existMatch(a))
   */   
  def notExistMatch(pattern: String) = existMatch(pattern).not
}
