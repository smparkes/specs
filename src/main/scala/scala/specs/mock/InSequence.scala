package scala.specs.mock
import scala.specs.Sugar._

case object inSequence extends inSequence
trait inSequence extends ProtocolType {
    def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String = {
      if (consume(expected, received) == (Nil, Nil))
        ""
      else
        failedProtocol(received)
    }
    def expectedDefs(expected: List[SpecifiedCall]) = {
      expected.map(_.expected).mkString("; ")
    }
    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (e::rest, rec) => {
          inAnyOrder.consume(List(e), rec) match {
            case (Nil, recRest) => consume(rest, recRest)
            case _ => (expected, received)
          }
        }
      }
    }
  }
