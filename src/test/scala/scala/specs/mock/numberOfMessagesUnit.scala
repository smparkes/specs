package scala.specs.mock;
import scala.specs.integration._
import scala.specs.Sugar._
import scala.specs.mock._

object numberOfMessagesSuite extends JUnit3(numberOfMessagesUnit)
object numberOfMessagesUnit extends Specification with ProtocolTypes {
  "A protocol type 'numberOfMessages'" should {
    val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
    val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
    "exactly 1: consume nothing if exp=m and rec=nil" in {
      new NumberOfMessages(exactlyN(1)).consume((e), ()) must_== (List(e), Nil)
    }
    "exactly 1: consume all if exp=m and rec=m" in {
      new NumberOfMessages(exactlyN(1)).consume((e), (r)) must_== (Nil, Nil)
    }
    "exactly 2: consume all if exp=m and rec=m, m" in {
      new NumberOfMessages(exactlyN(2)).consume(List(e), List(r, r)) must_== (Nil, Nil)
    }
    "exactly 2: consume nothing if exp=m and rec=m" in {
      new NumberOfMessages(exactlyN(2)).consume(List(e), List(r)) must_== (List(e), List(r))
    }
    "exactly 2 of m expects m, m" in {
      ProtocolDef(twoOf, List(e)).expects(List(r, r)) mustBe true
    }
  }
}
