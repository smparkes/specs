package scala.specs.mock
import scala.util.ExtendedList._
import scala.specs.Sugar._

case object inAnyOrder extends inAnyOrder

/**
 * The <code>inAnyOrder</code> protocol type will try to consume expected calls
 * in any order. It will not consume unexpected calls
 */
trait inAnyOrder extends ProtocolType {
    def constraints = "in any order"

    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (x::Nil, Nil) => (x::Nil, Nil)
        case (Nil, x::Nil) => (Nil, x::Nil)
        case (exp, rec) => {
          findShortestExpectedSequence(exp, rec) match {
            case Nil => (exp, rec)
            case y => consume(exp.removeFirst(_.expects(y)), rec.removeFirstSeq(y))
          }
        }
      }
    }
    def findShortestExpectedSequence(exp: List[SpecifiedCall], rec: List[ReceivedCall]): List[ReceivedCall]= {
      rec.sublists.sort((l1, l2) => l1.size < l2.size).find(rs => exp.exists(s => s expects rs)) match {
        case Some(x) => x
        case None => Nil
      }
    }

  }
