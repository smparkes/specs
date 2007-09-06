package scala.specs.matcher
import java.util.regex._

/**
 * This object provides utility functions for matchers
 */
object MatcherUtils {

  /**
   * returns true if b is matching the regexp a
   */
  def matches[T <: String](a: String)(b: T) = a != null && b != null && Pattern.compile(a).matcher(b).find 

  /**
   * returns true if a string s can be parsed to an integer
   */
  def isInteger(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}

  /**
   * return an object.toString() between quotes (used in messages creation)
   */
  def q(a: Any)  = if (a == null) "'null'" else "'" + a.toString + "'"
}
