package scala.specs.matcher
import scala.specs.mock._

/**
 * Matcher for mocks
 */
trait MockMatchers {
  /**
   * Matches if the expected messages are properly received
   */
  def beMet = new Matcher[Protocol](){
    def apply(protocol: => Protocol) = {
      val failures = protocol.failures
      (failures.isEmpty, "all expectations are met", failures)
    }
  }
  /**
   * <code>any()</code> can be used to match any parameter in a mock call
   */
  def any[T]: T = {
     null.asInstanceOf[T]
  }
}
