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
abstract class ProtocolType {

  /**
   * returns a string specifying if some expected calls have not been met
   * return None otherwise
   */
  def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): Option[String]
  def expectedDefs(expected: List[SpecifiedCall]): String
  def failedProtocol(received: List[ReceivedCall]) = "Failed protocol. " + messages(received)
  def messages(received: List[ReceivedCall]) = { 
    "Received" + (if (received.isEmpty) " none" else 
                     ":" + received.map {"\n  " + _.toString}.mkString(""))
  }
  def bracket(s: String) = "[" + s + "]"
  def unexpectedCalls(expected: List[SpecifiedCall], received: List[ReceivedCall]) = {
    consume(expected, received) match {
      case (Nil, Nil) => Nil
      case (_, unconsumed) => unconsumed
      case _ => Nil
    }      
  }
  def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall])
}

trait ProtocolTypes {
  object oneOf extends NumberOfMessages(exactlyN(1))
  object twoOf extends NumberOfMessages(exactlyN(2))
  object threeOf extends NumberOfMessages(exactlyN(3))
  object anyOf extends NumberOfMessages(atLeastN(0))
  object atLeastOneOf extends NumberOfMessages(atLeastN(1))
  object atMostOneOf extends NumberOfMessages(atMostN(1))
  case class atLeastNOf(n: Int) extends NumberOfMessages(atLeastN(n))
  case class exactlyNOf(n: Int) extends NumberOfMessages(exactlyN(n))
  case class atMostNOf(n: Int) extends NumberOfMessages(atMostN(n))

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
  }
  case class exactlyN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size == n
    def expectation: String = n + " of: "
  }
  case class atLeastN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size >= n
    def expectation: String = "at least " + n + " of: "
  }
  case class atMostN(n: Int) extends CallConstraint {
    def verifies(size: Int) = size <= n
    def expectation: String = "at most " + n + " of: "
  }
