package scala.specs.mock
import scala.specs.integration._
import scala.specs.Sugar._
import scala.specs.mock._
import scalacheck.Gen._
import scala.util.ExtendedList._
import scala.specs.matcher.ScalacheckParameters._

object inAnyOrderUnitSuite extends JUnit3(inAnyOrderUnit)
object inAnyOrderUnit extends Specification with TestData {
  "A protocol type 'inAnyOrder'" should {
   "consume nothing if exp=m and rec=nil" in {
      inAnyOrder.consume((e), ()) must_== (List(e), Nil)
    }
    "consume all if exp=m and rec=m" in {
      inAnyOrder.consume((e), (r)) must_== (Nil, Nil)
    }
    "consume one exp if exp=m1, m2 and rec=m2" in {
      inAnyOrder.consume(List(e1, e2), (r2)) must_== (List(e1), Nil)
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inAnyOrder.consume(List(e1, e2), List(r2, r1)) must_== (Nil, Nil)
    }
    "consume only one received message if exp=m and rec=m, m" in {
      inAnyOrder.consume(List(e), List(r, r)) must_== (Nil, List(r))
    }
    "find the shortest sequence of received calls that can be expected"   in {
      inAnyOrder.findShortestExpectedSequence(List(exactly2Calls), List(r, r)) must_== List(r, r)
    }
    "not consume received calls if it is a strict sublist of expected calls" in {
      val lessReceivedCalls = receivedSizeIs(_ < _)
      lessReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
        inAnyOrder.consume(expected, received)._1 must ((notBeEmpty).when(!expected.isEmpty) or 
                                                        (beEmpty).when(expected.isEmpty)) 
      }(set(maxSize->5))
    }
    "consume all received calls if it is a the same list of calls in a different order" in {
      val sameReceivedCalls = receivedSizeIs(_ == _)
      sameReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
        inAnyOrder.consume(expected, received) must_== (Nil, Nil) 
      }(set(maxSize->5))
    }
    "consume all expected calls if the received calls are a the superset of the expected calls" in {
      val moreReceivedCalls = receivedSizeIs(_ > _)
      moreReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
        inAnyOrder.consume(expected, received)._2 must ((notBeEmpty).when(!received.isEmpty) or 
                                                        (beEmpty).when(received.isEmpty)) 
      }(set(maxSize->5))
    }
  }
}
trait TestData extends ProtocolTypes {
  val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
  val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
  val exactly2Calls = new ProtocolDef(twoOf, List(e))

  val methods = List("m1", "m1", "m2", "m3")
  
  def receivedSizeIs(f: (Int, Int) => Boolean) = {
    for (expected <- listOf(elements(methods: _*));
         n <- choose(-2, 2);
         val received = (expected.scramble:::expected.scramble).take(expected.size + n) 
                        if (f(n + expected.size, expected.size)))
     yield (expected.map(new ExpectedCall(_)), received.map(new ReceivedCall(_)))
  }
}