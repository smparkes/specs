package scala.specs
import java.util.regex.Pattern
import scala.collection.mutable.Stack
import scala.specs.Sugar._

abstract sealed class ProtocolType {
  def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String
  def failedProtocol(received: List[ReceivedCall]) = "Failed protocol. " + messages(received)
  def messages(received: List[ReceivedCall]) = { 
    "Received" + (if (received.isEmpty) " none" else 
                     ":" + received.map {"\n  " + _.toString}.mkString(""))
  }
  def expectedDefs(expected: List[SpecifiedCall]): String
  def bracket(s: String) = "[" + s + "]"
  def unexpectedCalls(expected: List[SpecifiedCall], received: List[ReceivedCall]) = {
    consume(expected, received) match {
      case (Nil, Nil) => Nil
      case (_, unconsumed) => unconsumed
      case _ => Nil
    }      
  }
  def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall])
  def prefixes[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x::Nil => x::Nil
      case x::rest => List(x):::(prefixes(rest).map(x::_))
    }
  }
}

trait ProtocolTypes {
  case object inSequence extends ProtocolType with Sugar {
    def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String = {
      if (consume(expected, received) == (Nil, Nil))
        ""
      else
        failedProtocol(received)
    }
    def expectedDefs(expected: List[SpecifiedCall]) = {
      expected.map(_.expected).mkString("; ")
    }
    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (e::rest, rec) => {
          inAnyOrder.consume(List(e), rec) match {
            case (Nil, recRest) => consume(rest, recRest)
            case _ => (expected, received)
          }
        }
      }
    }
  }
  case object inAnyOrder extends ProtocolType with StringUtils with Sugar with ListUtils {
    def failures(expected: List[SpecifiedCall], received: List[ReceivedCall]): String = {
      if (consume(expected, received) == (Nil, Nil)) 
        ""
      else
        "Expected " + expectedDefs(expected) + ". " + messages(received)
    }
    def expectedDefs(expected: List[SpecifiedCall]) = {
      "in any order " + bracket(expected.map(_.expected).mkString("; "))
    }
    def consume(expected: List[SpecifiedCall], received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall]) = {
      (expected, received) match {
        case (Nil, Nil) => (Nil, Nil)
        case (x::Nil, Nil) => (x::Nil, Nil)
        case (Nil, x::Nil) => (Nil, x::Nil)
        case (exp, rec) => {
          findShortestExpectedSequence(exp, rec) match {
            case Nil => (exp, rec)
            case y => consume(exp.removeFirst(_.expects(y)), rec.removeFirstSeq(y))
          }
        }
      }
    }
    def findShortestExpectedSequence(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
      prefixes(rec).span(rs => !exp.exists(s => s expects rs))._2 match {
        case Nil => Nil
        case x::rest => x
      }
    }

  }
  object oneOf extends numberOfMessages(exactlyN(1))
  object twoOf extends numberOfMessages(exactlyN(2))
  object threeOf extends numberOfMessages(exactlyN(3))
  object anyOf extends numberOfMessages(atLeastN(0))
  object atLeastOneOf extends numberOfMessages(atLeastN(1))
  object atMostOneOf extends numberOfMessages(atMostN(1))
  case class atLeastNOf(n: Int) extends numberOfMessages(atLeastN(n))
  case class exactlyNOf(n: Int) extends numberOfMessages(exactlyN(n))
  case class atMostNOf(n: Int) extends numberOfMessages(atMostN(n))
  class numberOfMessages(c: CallConstraint) extends ProtocolType with StringUtils with Sugar {
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

  implicit def intToProtocolTypeBuilder(i: Int) = {
    new Object {
      def of = exactlyNOf(i)
      def atLeastOf = atLeastNOf(i)
      def atMostOf = atMostNOf(i)
    }
  }
}

class Protocol extends ProtocolTypes with Sugar {
  var receivedCalls: List[ReceivedCall] = Nil
  private var protocolDefs: Stack[ProtocolDef] = new Stack
  def expect(v:  => Any): ProtocolDef = expect(inAnyOrder)(v)
  def expect(t: ProtocolType)(v: => Any) = {
    protocolDefs.push(new ProtocolDef(t, Nil))
    v
    if (protocolDefs.size > 1) {
      val p = protocolDefs.pop
      protocolDefs.top.expect(p)
      p
    }
    else
      protocolDefs.top
  }
  def failures = protocolDef.failures(receivedCalls)
  def isSpecified = !protocolDefs.isEmpty
  def clear = {
    protocolDefs = new Stack
    receivedCalls = Nil
  }
  def protocolDef = protocolDefs.top
  def expectCall(methodName: String) = protocolDefs.top.expect(methodName)
  def receiveCall(methodName: String) = receivedCalls = receivedCalls:::List(new ReceivedCall(methodName))
}
abstract class SpecifiedCall {
  def expects(received: List[ReceivedCall]): Boolean
  def consume(received: List[ReceivedCall]): List[ReceivedCall]
  def expected: String
}
case class ExpectedCall(val method: String) extends SpecifiedCall {
  override def consume(received: List[ReceivedCall]) = received.dropWhile {method == _.method}
  override def expects(received: List[ReceivedCall]) = received.exists {method == _.method}
  override def expected = method
}
case class ReceivedCall(val method: String) {
  override def toString = method
}
case class ProtocolDef(val protocolType: ProtocolType, var expectedCalls: List[SpecifiedCall]) extends SpecifiedCall with ProtocolTypes {
  def expect(m: String) = expectedCalls = expectedCalls:::List(ExpectedCall(m))
  def expect(p: ProtocolDef) = expectedCalls = expectedCalls:::List(p)
  def failures(rs: List[ReceivedCall]): String = (unexpectedCallsFailures(rs):::List(unmatchedCallsFailures(rs))).mkString("\n")
  def unexpectedCallsFailures(rs: List[ReceivedCall]) = unexpectedCalls(rs).map(_.toString + " should not have been called")  
  def unmatchedCallsFailures(rs: List[ReceivedCall]) = protocolType.failures(expectedCalls, expectedReceivedCalls(rs))
  def expectedReceivedCalls(rs: List[ReceivedCall]) = rs.remove(unexpectedCalls(rs).contains(_))
  def unexpectedCalls(rs: List[ReceivedCall]) = protocolType.unexpectedCalls(expectedCalls, rs)
  override def expected = protocolType.expectedDefs(expectedCalls)
  override def expects(received: List[ReceivedCall]) = protocolType.failures(expectedCalls, received).isEmpty
  override def consume(received: List[ReceivedCall]) = protocolType.consume(expectedCalls, received)._2
}

trait Mocker extends ProtocolTypes with ExampleLifeCycle with MockMatchers {
  val protocol = new Protocol
  private var expectingMode = 0
  def expect(v: => Any): Protocol = expect(inAnyOrder)(v)
  def expect(t: ProtocolType)(v: => Any): Protocol = {
    if (expectingMode == 0) protocol.clear
    expectingMode += 1
    protocol.expect(t)(v); 
    expectingMode -= 1 
    protocol
  }
  def methodName = {
    val message = (new Exception()).getStackTrace.dropWhile(!_.toString.contains("record")).dropWhile(_.toString.contains("record"))(0).toString
    val matcherExp = Pattern.compile(".*\\.(.*\\(.*)").matcher(message)
    matcherExp.find
    matcherExp.group(1)
  }

  def record = {
    if (expectingMode > 0) 
      protocol.expectCall(methodName)
    else
      protocol.receiveCall(methodName)
  }
  override def beforeExample(ex: Example) = {
    protocol.clear
  } 
  override def afterTest(ex: Example) = {
    if (protocol.isSpecified) 
      (new Assert[Protocol](protocol, ex)) must beMet
  }
  override def afterExample(ex: Example) = {}
  implicit def protocolTypeToProtocolDef(t: ProtocolType)(v: => Any) = {
    expect(t)(v)
  }

}

