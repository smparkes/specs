package scala.specs.matcher;
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

trait StringMatchers extends AnyMatchers {
  
  def equalIgnoreCase[T <: String](a: T) = make[T]((b:T) => (a.equalsIgnoreCase(b), q(b) + " is equal ignoring case to " + q(a), q(b) + " is not equal ignoring case to " + q(a))) 
  def include[T <: String](a: String) = make[T]((b: T) => (b.indexOf(a) >= 0, q(b) + " includes " + q(a), q(b) + " doesn't include " + q(a))) 
  def beMatching[T <: String](a: T) = make[T]((b: T) => (matches(a)(b), q(b) + " matches " + q(a), q(b) + " doesn't match " + q(a))) 
  def startWith[T <: String](a: T) = make[T]((b: T) => (b.startsWith(a), q(b) + " starts with " + q(a), q(b) + " doesn't start with " + q(a))) 
  def endWith[T <: String](a: T) = make[T]((b: T) => (b.endsWith(a), q(b) + " ends with " + q(a), q(b) + " doesn't end with " + q(a))) 
  def notInclude[T <: String](a: T) = not[T](include[T](a)) 
  def notEqualIgnoreCase[T <: String](a: T) = not(equalIgnoreCase(a)) 
  def notBeMatching(a: String) = not[String](beMatching[String](a)) 
  def notStartWith(a: String) = not[String](startWith[String](a))
  def notEndWith(a: String) = not[String](endWith[String](a))
}
