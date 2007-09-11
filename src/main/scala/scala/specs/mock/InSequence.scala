package scala.specs.mock
import scala.specs.Sugar._
import scala.util.ExtendedList._

case object inSequence extends inSequence
trait inSequence extends ProtocolType {
    def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): Option[String] = {
      if (consume(expected, received) == (Nil, Nil))
        None
      else
        Some(failedProtocol(received))
    }
    def expectedDefs(expected: List[SpecifiedCall]) = {
      expected.map(_.expected).mkString("; ")
    }
    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (e::rest, rec) => {
          rec.prefixes.find(rs => e expects rs) match {
            case None => (expected, received)
            case Some(rs) => consume(rest, rec.removeFirstSeq(rs))
          }
        }
      }
    }
  }
