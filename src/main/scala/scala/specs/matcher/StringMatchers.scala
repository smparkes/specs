package scala.specs.matcher;
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>StringMatchers</code> trait provides matchers which are applicable to String objects
 */
trait StringMatchers {
  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def equalIgnoreCase[T <: String](a: T) = make[T]((b:T) => (a != null && b != null && a.equalsIgnoreCase(b), q(b) + " is equal ignoring case to " + q(a), q(b) + " is not equal ignoring case to " + q(a))) 

  /**
   * Matches if !(a.equalsIgnoreCase(b))
   */   
  def notEqualIgnoreCase[T <: String](a: T) = equalIgnoreCase(a).not 

  /**
   * Matches if (b.indexOf(a) >= 0)
   */   
  def include[T <: String](a: String) = make[T]((b: T) => (a != null && b != null && b.indexOf(a) >= 0, q(b) + " includes " + q(a), q(b) + " doesn't include " + q(a))) 

  /**
   * Matches if !(b.indexOf(a) >= 0)
   */   
  def notInclude[T <: String](a: T) = include[T](a).not 

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching[T <: String](a: T) = make[T]((b: T) => (matches(a)(b), q(b) + " matches " + q(a), q(b) + " doesn't match " + q(a))) 

  /**
   * Matches if b doesn't match the regular expression a
   */   
  def notBeMatching(a: String) = beMatching[String](a).not

  /**
   * Matches if b.startsWith(a)
   */   
  def startWith[T <: String](a: T) = make[T]((b: T) => (b!= null && a!= null && b.startsWith(a), q(b) + " starts with " + q(a), q(b) + " doesn't start with " + q(a))) 
  /**
   * Matches if !b.startsWith(a)
   */   
  def notStartWith(a: String) = startWith[String](a).not

  /**
   * Matches if b.endsWith(a)
   */   
  def endWith[T <: String](a: T) = make[T]((b: T) => (a != null && b != null && b.endsWith(a), q(b) + " ends with " + q(a), q(b) + " doesn't end with " + q(a))) 

  /**
   * Matches if !b.endsWith(a)
   */   
  def notEndWith(a: String) = endWith[String](a).not
}
