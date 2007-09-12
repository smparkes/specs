package scala.specs.mock
import scala.util.ExtendedList._
import scala.specs.Sugar._

case object atLeast extends atLeast 

/**
 * The <code>atLeast</code> protocol type will try to consume expected calls
 * in any order without paying attention to unexpected ones
 */
trait atLeast extends inAnyOrder {
  override def constraints = "at least"

  override def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
    super.consume(expected, received) match {
      case (Nil, Nil) => (Nil, Nil)
      case (Nil, x) => (Nil, Nil)
      case rest => rest
    }
  }
}
