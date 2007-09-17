package scala.specs.mock
import scala.util.ExtendedList._
import scala.specs.Sugar._

case object inAnyOrder extends inAnyOrder(exactlyN(1))

/**
 * The <code>inAnyOrder</code> protocol type will try to consume expected calls
 * in any order. It will not consume unexpected calls
 */
class inAnyOrder(val repetition: CallConstraint) extends ProtocolType(repetition) {
  def constraints = {
    repetition match{
      case exactlyN(n) if (n == 1) => "in any order" 
      case _ => repetition.expectation
    }
  }
  def consume(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    exp.foreach(_.repetition = repetition)
    var n = 0
    do {    
      exp foreach (_.consume(rec))
      n = n + 1
    } while (!repetition.verifies(n))
    (exp, rec)
  }
}
