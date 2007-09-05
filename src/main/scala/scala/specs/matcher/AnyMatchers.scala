package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>AnyMatchers</code> trait provides matchers which are applicable to any scala Reference or Value
 */
trait AnyMatchers {

  /**
   * Matches if (a eq b)
   */   
  def be[T](a: T) = make[T]((b: T) => (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef], q(b) + " is the same as " + q(a), q(b) + " is not the same as " + q(a))) 
  def notBe[T](a: T) = be(a).not
  
  /**
   * Matches if (a == b)
   */   
  def is_==[T](a: T) = make[T]((b:T) => ((a == b), q(b) + " is equal to " + q(a), q(b) + " is not equal to " + q(a))) 

  /**
   * Alias of is_==
   */   
  def be_==[T](a: T) = make[T]((b:T) => ((a == b), q(b) + " is equal to " + q(a), q(b) + " is not equal to " + q(a))) 

  /**
   * Matches if (a neq b)
   */   
  def notEq[T](a: T) = be(a).not 

  /**
   * Matches is (a != b)
   */   
  def is_!=[T](a: T) = (is_==(a)).not 
  
  /**
   * Matches if iterable.exists(_ == a)
   */   
  def beIn[T <: AnyRef](iterable: Iterable[T]) = make[T]((a: T) => (iterable.exists(_ == a), q(a) + " is in " + q(iterable), q(a) + " is not in " + q(iterable))) 

  /**
   * Matches if not(iterable.exists(_ == a))
   */   
  def notBeIn[T <: AnyRef](iterable: Iterable[T]) = beIn(iterable).not 

  /**
   * Matches if any object with an <code>isEmpty</code> method returns true: (Any {def isEmpty: Boolean}).isEmpty
   */   
  def beEmpty[S <: Any {def isEmpty: Boolean}] = make[S]((iterable: S) => (iterable.isEmpty, iterable + " is empty", iterable + " is not empty")) 

  /**
   * Matches if not(beEmpty(a))
   */   
  def notBeEmpty[S <: Any {def isEmpty: Boolean}] = beEmpty[S].not

  /**
   * Alias of notBeEmpty
   */   
  def isNotEmpty = notBeEmpty 

  /**
   * Alias of beEmpty
   */   
  def isEmpty = beEmpty
  def function[T](f: T => Boolean) = make((x: T) => (f(x), x + " verifies the property", x + " doesn't verify the expected property"))
}
