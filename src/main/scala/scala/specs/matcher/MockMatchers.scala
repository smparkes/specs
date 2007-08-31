package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.mock._

trait MockMatchers {
  def beMet = Matcher.make[Protocol](protocol =>
       (protocol.failures.isEmpty, "all expectations are met", protocol.failures)
  )
}
