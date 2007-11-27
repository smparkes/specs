package scala.specs.specification
import scala.specs._
import scala.specs.specification._
import scala.specs.runner.HtmlBizRunner
import scala.specs.runner.ConsoleRunner
import scala.util._
import scala.xml._
import scala.specs.Sugar._

object bizRunner extends ConsoleRunner(timerSpecificationSpec)
object timerSpecificationSpec extends BizSpecification with TestData {
  "The timer spec" is 
<p> 
   A Simple timer is an object which can measure time. Let's create a timer.
   When a timer is stopped{timer.stop.shh}, the timer should { "fail to return the elapsed time" in { 
                                                                    timer.hms must beMatching("\\d second")} } then
   {"return the elapsed time" in {timer.hms must beMatching("\\d second")}}
  
   A person can have its name reset. Let's set the person name to {Peter as person.setName _}, 
   then {"the person must be named " + Peter in {person.name must_== Peter}}
</p>
}
trait TestData {
  val timer = new SimpleTimer
  class Person {var name: String = ""; def setName(n: String) = name = n}
  val person = new Person; val Peter = "Peter"
}


