package scala.specs.matcher
import java.util.regex._

object MatcherUtils {
  def matches[T <: String](a: String)(b: T) = Pattern.compile(a).matcher(b).find 
  def isInteger(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def q(a: Any)  = if (a == null) "'null'" else "'" + a.toString + "'"
  def indent(s: String) = s + "  "
  implicit def stringToQuotable(s: String) = new Object {def quote = q(s)}
}
