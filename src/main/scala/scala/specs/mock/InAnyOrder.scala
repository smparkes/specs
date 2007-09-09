package scala.specs.mock
import scala.util.ExtendedList._
import scala.specs.Sugar._

case object inAnyOrder extends inAnyOrder
trait inAnyOrder extends ProtocolType {
    def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String = {
      if (consume(expected, received) == (Nil, Nil)) 
        ""
      else
        "Expected " + expectedDefs(expected) + ". " + messages(received)
    }
    def expectedDefs(expected: List[SpecifiedCall]) = {
      "in any order " + bracket(expected.map(_.expected).mkString("; "))
    }
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
    def findShortestExpectedSequence(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
      prefixes(rec).sort((l1, l2) => l1.size < l2.size).find(rs => exp.exists(s => s expects rs)) match {
        case None => Nil
        case Some(x) => x
      }
    }

  }
