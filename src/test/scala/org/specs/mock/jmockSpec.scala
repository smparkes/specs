package org.specs.mock
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._

object jmockSpecRunner extends ConsoleRunner(jmockSpec)
object jmockSpec extends Specification with ButtonAndLightJMock {
  "A class" should {
    "throw a Failure exception if no expected call is received" in {
      expectedCall.failures.size must be_>(0)
    } 
    "be mockable using JMock" in {
      expect {
        one(light).on
      }
      button.push
    } 
  }
}
trait ButtonAndLightJMock extends ButtonAndLight with JMocker with ClassMocker {
  val light = mock(classOf[Light])
  val button = Button(light)

  object expectedCall extends Specification with JMocker with ClassMocker {
    "the light" should {
      "miss one expected call" in {
        expect { one(light).on }
      } 
    }    
  } 
}
