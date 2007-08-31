package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

trait LogicalMatchers {

  def not[T](m: Matcher[T]) = m.not
  def verifyAll[T](ms: Matcher[T]*): Matcher[T] = verifyAll(ms.toList)
  def verifyAll[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => make[T]((a: T) => (true, "no matchers", "no matchers"))
      case m::rest => m.and(verifyAll(rest))
    }
  }
  def verifyAny[T](ms: Matcher[T]*): Matcher[T] = verifyAny(ms.toList)
  def verifyAny[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => make[T]((a: T) => (false, "no matchers", "no matchers"))
      case m1::m2::Nil => m1.or(m2)
      case m::rest => m.or(verifyAny(rest))
    }
  }
}
