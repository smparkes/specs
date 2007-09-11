package scala.specs.mock
import scala.specs.matcher._
import scala.specs.Sugar._
import scala.collection.mutable.Stack
import java.util.regex.Pattern

/**
 * This class
 */
class Protocol extends ProtocolTypes {
  var receivedCalls: List[ReceivedCall] = Nil
  private var protocolDefs: Stack[ProtocolDef] = new Stack
  def expect(v:  => Any): ProtocolDef = expect(atLeast)(v)
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
  def expectCall(methodName: String) = {
    protocolDefs.top.expect(methodName)
  }
  def receiveCall(methodName: String) = {
    receivedCalls = receivedCalls:::List(new ReceivedCall(methodName))
  }
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
  def failures(rs: List[ReceivedCall]): String = {
    (unmatchedCallsFailures(rs) match {
      case Some(unmatched) => unexpectedCallsFailures(rs):::List(unmatched)
      case None => unexpectedCallsFailures(rs)
    }).mkString("\n")
  }
  def unexpectedCallsFailures(rs: List[ReceivedCall]) = unexpectedCalls(rs).map(_.toString + " should not have been called")  
  def unmatchedCallsFailures(rs: List[ReceivedCall]) = protocolType.failures(expectedCalls, expectedReceivedCalls(rs))
  
  def expectedReceivedCalls(rs: List[ReceivedCall]) = rs.remove(unexpectedCalls(rs).contains(_))
  def unexpectedCalls(rs: List[ReceivedCall]) = {
    protocolType.unexpectedCalls(expectedCalls, rs)
  }
  override def expected = protocolType.expectedDefs(expectedCalls)
  override def expects(received: List[ReceivedCall]) = protocolType.failures(expectedCalls, received).isEmpty
  override def consume(received: List[ReceivedCall]) = protocolType.consume(expectedCalls, received)._2
}

trait Mocker extends ProtocolTypes with ExampleLifeCycle with MockMatchers {
  val protocol = new Protocol
  private var expectingMode = 0
  def expect(v: => Any): Protocol = expect(atLeast)(v)
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

  def recordAndReturn[T](v: T): T = {
    record
    v
  }
  def record[T](v: T): T = {
    record
    v
  }
  def record: Unit = {
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
    protocol.clear
  }
  override def afterExample(ex: Example) = {
    protocol.clear
  }
  implicit def protocolTypeToProtocolDef(t: ProtocolType)(v: => Any) = {
    expect(t)(v)
  }

}

