package scala.specs.matcher
import org.specs.matcher.MatcherUtils._
import org.collection.ExtendedIterable._

/**
 * The <code>IterableMatchers</code> trait provides matchers which are applicable to Iterable objects
 */
trait IterableMatchers {
   
  /**
   * Matches if (iterable.exists(_ == a)
   */   
  def contain[T](a: T) = new Matcher[Iterable[T]](){ 
     def apply(iterable: => Iterable[T]) = (iterable.exists(_ == a), q(iterable) + " contains " + q(a), q(iterable) + " doesn't contain " + q(a)) 
   }

  /**
   * Matches if not(iterable.exists(_ == a)
   */   
  def notContain[T](a: T) = contain(a).not 

  /**
   * Matches if there is one element in the iterable verifying the <code>function</code> parameter: <code>(iterable.exists(function(_))</code>
   */   
  def exist[T](function: T => Boolean) = new Matcher[Iterable[T]](){ 
     def apply(iterable: => Iterable[T]) = (iterable.exists{function(_)}, "at least one element verifies the property in " + q(iterable), "no element verifies the property in " + q(iterable)) 
  }

  /**
   * Matches if there is no element in the iterable verifying the <code>function</code> parameter: <code>!(iterable.exists(function(_))</code>
   */   
  def notExist[T](function: T => Boolean) = exist(function).not 

  /**
   * Matches if there is one element in the iterable[String] matching the <code>pattern</code> parameter: <code> iterable.exists(matches(pattern) _)</code>
   */   
  def existMatch(pattern: String) = new Matcher[Iterable[String]](){
     def apply(iterable: => Iterable[String]) = (iterable.exists( matches(pattern) _), "at least one element matches " + q(pattern) + " in " + q(iterable), "no element matches " + q(pattern) + " in " + q(iterable))
  }

  /**
   * Alias for existMatch
   */   
  def containMatch(pattern: String) = existMatch(pattern) 

  /**
   * Matches if not(existMatch(a))
   */   
  def notExistMatch(pattern: String) = existMatch(pattern).not

  /**
   * Matches if there l contains the same elements as the Iterable <code>iterable</code>.<br>
   * This verification does not consider the order of the elements but checks the iterables recursively
   */   
  def haveSameElementsAs[T](l: Iterable[T]) = new Matcher[Iterable[T]](){
    def apply(iterable: => Iterable[T]) = (
        l.sameElementsAs(iterable),
        q(l.toDeepString) + " has the same elements as " + q(iterable.toDeepString), 
        q(l.toDeepString) + " hasn't the same elements as " + q(iterable.toDeepString))
  }

}
