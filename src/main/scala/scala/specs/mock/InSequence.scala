package scala.specs.mock
import scala.specs.Sugar._
import scala.util.ExtendedList._

case object inSequence extends inSequence

/**
 * The <code>atLeast</code> protocol type will try to consume expected calls
 * in the order they are specified. It will not consume unexpected calls
 */
trait inSequence extends ProtocolType {
    // "in sequence" is the default constraint  
    def constraints = "" 
    
    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (Nil, rec) => (expected, received)
        case (e::rest, rec) => {
          rec.prefixes.find(rs => e expects rs) match {
            case None => (expected, received)
            case Some(rs) => consume(rest, rec.removeFirstSeq(rs))
          }
        }
      }
    }
  }
