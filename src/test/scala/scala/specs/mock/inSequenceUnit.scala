package scala.specs.mock
import scala.specs.integration._
import scala.specs.Sugar._
import scala.specs.mock._
import scalacheck.Gen._
import scala.util.ExtendedList._
import scala.specs.matcher.ScalacheckParameters._

object inSequenceSuite extends JUnit3(inSequenceUnit)
object inSequenceUnit extends Specification with TestData {
  "A protocol type 'inSequence'" should {
    val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
    val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
    "consume all if exp=m and rec=m" in {
      inSequence.consume((e), (r)) must_== (Nil, Nil)
    }
    "consume the first exp if exp=m1, m2 and rec=m1" in {
      inSequence.consume(List(e1, e2), (r1)) must_== (List(e2), Nil)
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inSequence.consume(List(e1, e2), List(r1, r2)) must_== (Nil, Nil)
    }
  }
  "not consume received calls if it is a strict sublist of expected calls" in {
    val lessReceivedCalls = receivedSizeIs(_ < _)
    lessReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      inSequence.consume(expected, received)._1 must ((notBeEmpty).when(!expected.isEmpty) or 
                                                      (beEmpty).when(expected.isEmpty)) 
    }(set(maxSize->5))
  }
  "consume all received calls if it is a the same list of calls in the same order" in {
    val sameReceivedCalls = receivedSizeIs(_ == _)
    sameReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      inSequence.consume(expected, received) must be_==((Nil, Nil)).when(expected == received) 
    }(set(maxSize->5))
  }
  "consume all expected calls if they are a prefix of received calls" in {
    val moreReceivedCalls = receivedSizeIs(_ > _)
    moreReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      val (consumedExpected, consumedReceived) = inSequence.consume(expected, received)
      val receivedStartsWithExpected = received.map(_.method).startsWith(expected.map(_.method))
      consumedExpected must (beEmpty.when(received.isEmpty || receivedStartsWithExpected) and 
                             notBeEmpty.when(!receivedStartsWithExpected)) 
      consumedReceived must (notBeEmpty.when(!received.isEmpty && receivedStartsWithExpected) and 
                             beEmpty.when(received.isEmpty)) 
    }(set(maxSize->5))
  }
}
