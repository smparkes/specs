package scala.specs.matchers
import scala.specs.integration._


object mockMatchersSuite extends JUnit3TestSuite(mockMatchers) 
object mockMatchers extends MatchersSpecification {
  import ButtonAndLight._
  "Mock matchers" should { usingBefore { () => {clearExample; protocol = new Protocol} }
    "provide an 'expect' matcher checking if calls have been made to mock objects" in {
      val mock = new Light { 
        override def on = record 
        override def off = record 
      }
      val button = Button(mock)
      // by default, the calls can be made in any order
      val protocol = expect { 
                       mock.off 
                       mock.on 
                     }

      assertion(protocol must beMet) must (failWithMatch("off\\(.*\\) should have been called") and 
                                           failWithMatch("on\\(.*\\) should have been called"))

      button.push
      assertion(protocol must beMet) must failWithMatch("off\\(.*\\) should have been called")

      button.push
      // the protocol is always checked at the end of an example
    }
    "provide an 'expect inSequence' matcher checking if calls have been made to mock objects inSequence" in {
      val mock = new Light { 
        override def on = record 
        override def off = record 
      }
      val button = Button(mock)
      val protocol = expect(inSequence) { 
                       mock.off 
                       mock.on 
                     }
      button.push
      button.push
      assertion(protocol must beMet) must failWithMatch("Unmatched protocol. Received:\n  on\\(.*\\)\n  off\\(.*\\)")
    }
  }
}
object ButtonAndLight {
  case class Button(light: Light) {
    var lightOn = false
    def push = {
      if (lightOn) light.off else light.on 
      lightOn = !lightOn
    }
  }
  case class Light {
    var state: LightState = Off
    def on = state = On
    def off = state = Off
    def isOn = state == On
  }
  abstract sealed class LightState(s: String)
  case class On extends LightState("on")
  case class Off extends LightState("off")
}

