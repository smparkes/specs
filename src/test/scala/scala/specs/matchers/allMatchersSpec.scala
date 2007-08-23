package scala.specs.matchers
import scala.collection.mutable.Queue
import scala.specs.integration._

object matchersTestSuite extends JUnit3TestSuite(matchersSpec) 
object matchersSpec extends Specification {
  "Matchers" areSpecifiedBy (objectMatchers, 
                             stringMatchers, 
                             iterableMatchers, 
                             mapMatchers,
                             patternMatchers,
                             mockMatchers)
}
trait MatchersSpecification extends Specification with Sugar {
  var reported: Example = new Example("this example serves as a stub to collect failure messages", new Sut(""))
  def clearExample = { reported = new Example("", new Sut("")) }
  def failWith(message: String) = is_==(message)
  def failWithMatch(pattern: String) = beMatching(pattern)
  def assertion(value: => Any): String = {
    try {
      value
    } catch {
      case FailureException(message) => return message
      case t: Throwable => throw t
    }
    return "this assertion has not failed"
  }
}
