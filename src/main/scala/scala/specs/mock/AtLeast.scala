package scala.specs.mock
import scala.util.ExtendedList._
import scala.specs.Sugar._

case object atLeast extends atLeast 
trait atLeast extends inAnyOrder {
    override def expectedDefs(expected: List[SpecifiedCall]) = {
      "atLeast " + bracket(expected.map(_.expected).mkString("; "))
    }
    override def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (x::Nil, Nil) => (x::Nil, Nil)
        case (Nil, x::rest) => (Nil, Nil)
        case (exp, rec) => {
          findShortestExpectedSequence(exp, rec) match {
            case Nil => (exp, rec)
            case y => consume(exp.removeFirst(_.expects(y)), rec.removeFirstSeq(y))
          }
        }
      }
    }
  }
