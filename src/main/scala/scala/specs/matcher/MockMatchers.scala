package scala.specs.matcher
import scala.specs.mock._

trait MockMatchers {
  def beMet = new Matcher[Protocol](){
    def apply(protocol: => Protocol) = (protocol.failures.isEmpty, "all expectations are met", protocol.failures)
  }
}
