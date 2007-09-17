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
   * returns error messages specifying if some expected calls have not been met.
   * If exclusive is true, then return also an error message when unexpected calls occured
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
  
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[inAnyOrder]) 
      false
    else
      other.asInstanceOf[inAnyOrder].repetition == repetition
  }
}

sealed case class Exclusivity(isExclusive: Boolean)
object exclusively extends Exclusivity(true)
object nonExclusively extends Exclusivity(false)
case class atLeastNOf(n: Int) extends inAnyOrder(atLeastN(n))
case class exactlyNOf(n: Int) extends inAnyOrder(exactlyN(n))
case class atMostNOf(n: Int) extends inAnyOrder(atMostN(n))

/**
 * This trait adds some frequent protocol types and some
 * syntactic sugar to be able to specify such protocol types
 * anyOf = inAnyOrder(exactlyN(0))
 * oneOf = inAnyOrder(exactlyN(1))
 * 2.of = inAnyOrder(exactlyN(2))
 * 3.atLeastOf = inAnyOrder(exactlyN(3))
 * 3.inSequenceAtMostOf = inAnyOrder(exactlyN(3))
 */
trait ProtocolTypes {
  def oneOf = new inAnyOrder(exactlyN(1))
  def twoOf = new inAnyOrder(exactlyN(2))
  def threeOf = new inAnyOrder(exactlyN(3))
  def anyOf = new inAnyOrder(atLeastN(0))
  def atLeastOneOf = new inAnyOrder(atLeastN(1))
  def atMostOneOf = new inAnyOrder(atMostN(1))
  implicit def intToProtocolTypeBuilder(i: Int) = {
    
    new Object {
      def of = exactlyNOf(i)
      def atLeastOf = atLeastNOf(i)
      def atMostOf = atMostNOf(i)
      def inSequenceOf = new inSequence(exactlyN(i))
      def inSequenceAtLeastOf = new inSequence(atLeastN(i))
      def inSequenceAtMostOf = new inSequence(atMostN(i))
    }
  }
}

