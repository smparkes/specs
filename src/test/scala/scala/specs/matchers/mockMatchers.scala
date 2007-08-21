package scala.specs.matchers
import scala.specs.integration._
import java.util.regex.Pattern

object mockMatchersSuite extends JUnit3TestSuite(mockMatchers) 
object mockMatchers extends MatchersSpecification {
  abstract sealed class Frequency
  case object once extends Frequency
  case object twice extends Frequency
  var expectations : List[Expectation]= Nil
  var frequency: Frequency = _
  var recordingMode = false
  def expect(f: Frequency)(v: => Any) = {
    recordingMode = true
    frequency = f
    v
    recordingMode = false
  }
  def record = {
    if (recordingMode)
      expectations = new ExpectedCall(methodName, frequency)::expectations
    else {
    expectations.find { case ExpectedCall(methodName, _) => true } match {
      case Some(e: ExpectedCall) => e.call
      case _ => new UnexpectedCall(methodName)::expectations 
    }}
  }
  def methodName = {
    val message = (new Exception()).getStackTrace.drop(2)(0).toString
    val matcherExp = Pattern.compile(".*\\.(.*)\\(").matcher(message)
    matcherExp.find
    matcherExp.group(1)
  }
  def beMet = Matcher.make[Iterable[Expectation]](expectations =>
    (expectations.forall(_.check), "all expectations are met", expectations.map { _.failure }.mkString("\n"))
  )
  abstract sealed class Expectation {
    def check: Boolean
    def failure: String
  }
  class ExpectedCall(val method: String, val frequency: Frequency) extends Expectation {
    var callNb = 0
    var failure = ""
    def call = callNb += 1
    def check = frequency match {
      case x if (x == once && callNb == 1) => true
      case x if (x == once && callNb == 0) => {failure = "The method " + q(method) + " has not been called once"; false}
      case _ => false
    }
  }
  object ExpectedCall {
    def unapply(e: ExpectedCall): Option[(String, Frequency)]= Some((e.method, e.frequency))
  }
  class UnexpectedCall(val method: String) extends Expectation {
    var failure = method + " has been called, but was not expected"
    def check = false
  }
  object UnexpectedCall {
    def unapply(e: UnexpectedCall): Option[(String)]= Some((e.method))
  }
  "Mock matchers" should { usingBefore { () => clearExample }
    "provide an 'expect(once)' matcher checking if an object has received a method call" in {
      val mock = new Light { override def on = record }
      expect(once) { mock.on }

      val button = Button(mock)
      assertion(expectations must beMet) must failWith("The method 'on' has not been called once")

      button.push
      expectations must beMet
    }
  }
  
  case class Button(light: Light) {
    def push = light.on
  }
  case class Light {
    var state: LightState = Off
    def on = state = On
    def off = state = Off
  }
  abstract sealed class LightState(s: String)
  case class On extends LightState("on")
  case class Off extends LightState("off")
}
