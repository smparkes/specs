package scala.specs.matcher
import scala.specs.mock._

trait MockMatchers {
  def beMet = new Matcher[Protocol](){
    def apply(protocol: => Protocol) = {
      val failures = protocol.failures
      (failures.isEmpty, "all expectations are met", failures)
    }
  }
  def any[T]: T = {
     null.asInstanceOf[T]
  }
}
