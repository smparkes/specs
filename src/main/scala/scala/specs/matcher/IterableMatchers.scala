package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

trait IterableMatchers extends AnyMatchers {
  def contain[T <: AnyRef](a: T) = make[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists(_ == a), iterable + " contains " + q(a), iterable + " doesn't contain " + q(a))) 
  def exist[T <: AnyRef](function: T => Boolean) = make[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists{function(_)}, "at least one element verifies the property in " + iterable, "no element verifies the property in " + iterable)) 
  def existMatch(pattern: String) = make[Iterable[String]]((iterable: Iterable[String]) => (iterable.exists( matches(pattern) _), "at least one element matches " + q(pattern) + " in " + iterable, "no element matches " + q(pattern) + " in " + iterable)) 
  def notContain[T <: AnyRef](a: T) = not(contain(a)) 
  def notExist[T <: AnyRef](function: T => Boolean) = not(exist(function)) 
  def containMatch(pattern: String) = existMatch(pattern) 
  def notExistMatch(pattern: String) = not(existMatch(pattern))
}
