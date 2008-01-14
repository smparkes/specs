package org.specs.mock
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._

class jmockSpecTest extends JUnit3(jmockSpec)
object jmockSpecRunner extends ConsoleRunner(jmockSpec)
object jmockSpec extends Specification {
  "The test spec" should {
    "have one failure" in {
      testSpec.failures.map(_.getMessage) must existMatch("not all expectations were satisfied")
    } 
    "have 0 errors" in {
      testSpec.errors.size must_== 0
    } 
  }
}
object testSpec extends Specification with ButtonAndLightJMock {
  "this test system" should {
    usingBefore { createMocks _}
    "miss one expected call" in {
      expect { one(light).on }
    }
    "get one expected call" in {
      expect { one(light).on }
      button.push
    } 
  }    
} 

trait ButtonAndLightJMock extends ButtonAndLight with JMocker with ClassMocker {
  var light: Light = _ 
  var button: Button = _
  def createMocks = {
    light = mock(classOf[Light])
    button = Button(light)
  }
}
