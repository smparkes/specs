package scala.specs.mock
import scala.specs.Sugar._
import scala.specs._
import scala.specs.matcher._
import scala.util.ExtendedList._
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>ProtocolType</class> specifies if a sequence of <code>ReceivedCall</code> can match
 * a sequence of <code>SpecifiedCall</code>
 */
abstract class ProtocolType(repetition: CallConstraint) {

  /**
   * A string describing the constraints of this protocol type
   * it must be implemented by subclasses to provide a meaningful name to describe the protocol
   * in error messages
   */
  def constraints: String

  /**
   * Consumes the expected messages with the actual received ones
   * If the expected messages are not all consumed, there will be a failure message
   */
  def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall])
  
  /**
   * returns error messages specifying if some expected calls have not been met
   * or if some unexpected calls happened
   * return "" otherwise
   */
   def failures(expected: List[SpecifiedCall], received: List[ReceivedCall], exclusive: Boolean): String = {
     consume(expected, received)
     if (expected.forall(_.passes) && ((exclusive && received.forall(_.consumed)) ||
                                      !exclusive))   
       ""
     else 
       "Expected " + expectedDefs(expected) + ". " + receivedMessages(received)
   }

  /**
   * returns a user message specifying the protocol constraints on the expected calls:
   * for example "in any order m1; m2" or "in sequence m1; m2; m3"
   */
  def expectedDefs(expected: List[SpecifiedCall]): String = {
     constraints + (if (!constraints.isEmpty) " " else "") + expected.mkString("[", "; ", "]")
  }

  /**
   * returns a user message with the list of received messages
   */
  def receivedMessages(received: List[ReceivedCall]) = { 
    "Received" + (if (received.isEmpty) " none" else 
                     ":" + received.map {"\n  " + _.toString}.mkString(""))
  }
}

trait ProtocolTypes {
  
  sealed case class Exclusivity(isExclusive: Boolean)
  val exclusively = Exclusivity(true)
  val nonExclusively  = Exclusivity(false)

  object oneOf extends inAnyOrder(exactlyN(1))
  object twoOf extends inAnyOrder(exactlyN(2))
  object threeOf extends inAnyOrder(exactlyN(3))
  object anyOf extends inAnyOrder(atLeastN(0))
  object atLeastOneOf extends inAnyOrder(atLeastN(1))
  object atMostOneOf extends inAnyOrder(atMostN(1))
  case class atLeastNOf(n: Int) extends inAnyOrder(atLeastN(n))
  case class exactlyNOf(n: Int) extends inAnyOrder(exactlyN(n))
  case class atMostNOf(n: Int) extends inAnyOrder(atMostN(n))

  implicit def intToProtocolTypeBuilder(i: Int) = {
    new Object {
      def of = exactlyNOf(i)
      def atLeastOf = atLeastNOf(i)
      def atMostOf = atMostNOf(i)
    }
  }
}
  abstract sealed class CallConstraint { 
    def verifies(size: Int): Boolean
    def expectation: String
    def stop(n: Int): Boolean = verifies(n)
  }
  case class exactlyN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size == n
    def expectation: String = n + " of:"
  }
  case class atLeastN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size >= n
    def expectation: String = "at least " + n + " of:"
    override def stop(n: Int): Boolean = false
  }
  case class atMostN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size <= n
    def expectation: String = "at most " + n + " of:"
  }
