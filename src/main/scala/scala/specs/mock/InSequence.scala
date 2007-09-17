package scala.specs.mock
import scala.specs.Sugar._
import scala.util.ExtendedList._

case object inSequence extends inSequence(exactlyN(1))

/**
 * The <code>atLeast</code> protocol type will try to consume expected calls
 * in the order they are specified. It will not consume unexpected calls
 */
class inSequence(repetition: CallConstraint) extends ProtocolType(repetition) {
   def constraints = {
     repetition match{
       case exactlyN(n) if (n == 1) =>  "in sequence"
       case _ => "in sequence" + repetition.expectation
     }
   }
  
  def consume(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    exp.foreach(_.repetition = repetition)
    var n = 0
    do {    
      exp foreach (_.consume(rec))
      if (rec.exists(r1 =>
      		r1.consumed && rec.exists(r2 =>
            r2.consumed && 
            (exp.indexOf(r1.consumedBy.get) > exp.indexOf(r2.consumedBy.get)) &&
            (rec.indexOf(r1) < rec.indexOf(r2))  
          )
      )) {
        rec foreach (_.consumedBy = None)
      }
      n = n + 1
    } while (!repetition.verifies(n))
    (exp, rec)
  }
}
