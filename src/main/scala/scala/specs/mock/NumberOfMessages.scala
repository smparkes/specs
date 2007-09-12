package scala.specs.mock;

/**
 * The <code>NumberOfMessages</code> protocol type will try to consume expected calls
 * at least, at most or exactly a given number of times
 */
class NumberOfMessages(c: CallConstraint) extends ProtocolType {
  def constraints = c.expectation

  def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]) = {
    consume(expected, received, 1) match {
      case Some(n) if (c.verifies(n)) => (Nil, Nil)
      case _ => (expected, received)
    }
  }
  def consume(expected: List[SpecifiedCall], received: List[ReceivedCall], n: Int): Option[Int] = {
    inAnyOrder.consume(expected, received) match {
      case (Nil, Nil) => Some(n)
      case (Nil, rec) => consume(expected, rec, n + 1)
      case (exp, Nil) => None
      case (exp, rec) => None
    }
  }
}
