package org.specs.util

import org.specs._
import org.specs.runner._

object timerSuite extends JUnit3(timerSpec)
object timerSpec extends Specification {
  "A timer" should {
    "display 0 seconds if not stopped after being created" in { 
      TestTimer().hms must_== "0 second"
    }
    "display the elapsed time if stopped after being created" in { 
      val timer = TestTimer()
      Thread.sleep(1500)
      timer.stop
      timer.hms must_== "1 second"
      timer.preciseTime must beMatching("1 second, \\d+ ms")
    }
    "returns the elapsed time with its stop method" in { 
      val timer = TestTimer()
      Thread.sleep(1000)
      timer.stop mustMatch "1 second"
    }
  }
  case class TestTimer extends SimpleTimer
}
