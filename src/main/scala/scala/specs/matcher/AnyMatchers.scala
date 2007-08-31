package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

trait AnyMatchers extends LogicalMatchers {
  def be[T](a: T) = make[T]((b: T) => (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef], q(b) + " is the same as " + q(a), q(b) + " is not the same as " + q(a))) 
  def is_==[T](a: T) = make[T]((b:T) => ((a == b), q(b) + " is equal to " + q(a), q(b) + " is not equal to " + q(a))) 
  def beIn[T <: AnyRef](iterable: Iterable[T]) = make[T]((a: T) => (iterable.exists(_ == a), q(a) + " is in " + iterable, q(a) + " is not in " + iterable)) 
  def notBeIn[T <: AnyRef](iterable: Iterable[T]) = not(beIn(iterable)) 
  def beEmpty[S <: Any {def isEmpty: Boolean}] = make[S]((iterable: S) => (iterable.isEmpty, iterable + " is empty", iterable + " is not empty")) 
  def notBe[T](a: T) = not(be(a)) 
  def notEq[T](a: T) = not(be(a)) 
  def is_!=[T](a: T) = not(is_==(a)) 
  def notBeEmpty[S <: Any {def isEmpty: Boolean}] = not(beEmpty[S])
  def isEmpty = beEmpty
  def isNotEmpty = notBeEmpty 
  def function[T](f: T => Boolean) = make((x: T) => (f(x), x + " verifies the property", x + " doesn't verify the expected property"))
}
