package scala.specs.mock
import scala.specs.integration._
import scala.specs.Sugar._
import scala.specs.mock._

object inSequenceSuite extends JUnit3(inSequenceUnit)
object inSequenceUnit extends Specification with ProtocolTypes {
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
}
