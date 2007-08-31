package scala.specs.mock;

class NumberOfMessages(c: CallConstraint) extends ProtocolType {
  def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String = {
    consume(expected, received) match {
      case (Nil, Nil) => ""
      case _ => "Expected " + expectedDefs(expected) + ". " + messages(received)
    }
  }
  def expectedDefs(expected: List[SpecifiedCall]) = {
    c.expectation + bracket(expected.map(_.expected).mkString("; "))
  }
  override def unexpectedCalls(expected: List[SpecifiedCall], received: List[ReceivedCall]) = Nil
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
