package org.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._

object matchersTestSuite extends JUnit3(matchersSpec) 
object matchersSpec extends Specification {
  "Matchers" areSpecifiedBy (objectMatchersSpec, 
                             stringMatchersSpec, 
                             iterableMatchersSpec, 
                             mapMatchersSpec,
                             patternMatchersSpec,
                             scalacheckMatchersSpec)
}
class MatchersSpecification extends Specification {
  var reported: Example = new Example("this example serves as a stub to collect failure messages", new Sut("", this))
  def clearExample = { reported = new Example("", new Sut("", this)) }
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
