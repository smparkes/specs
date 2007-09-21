package scala.specs.matcher
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>PatternMatchers</code> trait provides matchers which allow to use pattern matching
 * to match expressions
 */
trait PatternMatchers {
  
  /**
   * Matches if the value 'v' is like the pattern { case expression => boolean_result }<br>
   * Uses the fact that we can use the following syntax to map Options:
   *  </code> myOption.map { case expression => boolean result }<code><br>
   * In that case,  <code>{ case expression => boolean_result }</code> = pattern, is a function <code>Any => Boolean</code><br>
   * If there is no match, return false and if there is a match, return the value inside the mapped option, which should be 
   * <code>true</code><br>
   * The <code>Sugar</code> object can be used to get shorter expression by having the <code>ok</code> alias for <code>true</code>:
   *   <code>List(1, 2) must beLike { case x::y::Nil => ok }</code>
   * @param pattern a case expression
   */  
  def beLike(pattern: => (Any => Boolean)) = new Matcher[Any](){
     def apply(value: => Any) = ( 
      try {
        if (value == null)
          false
        else
          Some(value).map(pattern).get 
      } catch { case e: scala.MatchError => false }, 
      q(value) + " matches the given pattern", 
      q(value) + " doesn't match the expected pattern")
  }
  
  /**
   * Matches if the value 'v' is None
   */
  def beNone[T] = new Matcher[Option[T]](){
     def apply(value: => Option[T]) = ( 
      value match { 
        case None => true
        case _ => false 
      }, 
      q(value) + " is None", 
      q(value) + " is not None")
  }

  /**
   * Matches if the value 'v' is Some(x)
   */
  def beSome[T] = new CaseMatcher[T](){
     def someApply(value: => Option[T]) = ( 
       value match {
          case Some(x) => true 
          case _ => false
        },
        q(value) + " is Some(x)", 
        q(value) + " is not Some(x)")
   }

  /**
   * Alias for beSome[Any]
   */
  def beSomething = beSome[Any]


  /**
   * The CaseMatcher class allow to verify expressions such as:
   * <code>Some(x) must beSome[String].which(_.startWith("abc"))</code>
   */
  abstract class CaseMatcher[T] extends Matcher[Option[T]] {
    private var whichFunction: Option[T => Boolean] = None
    def which(g: T => Boolean) = {
      whichFunction = Some(g) 
      this
    }
    def someApply(value: => Option[T]): (Boolean, String, String)
    override def apply(a: => Option[T]) = 
      if (whichFunction == Some(null))
        (false, "the 'which' property is a not a null function", "the 'which' property is a null function")
      else
        whichFunction match {
          case None => someApply(a)
          case Some(g) => ( a match {
                          case Some(x) => g(x) 
                          case _ => false
                        },
                        "there is a Some(x) verifying the given property", 
                        "there is no Some(x) verifying the given property")
    }
  }
}
/**
 * Companion object for PatternMatchers
 */
object PatternMatchers extends PatternMatchers
