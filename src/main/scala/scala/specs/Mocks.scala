package scala.specs
import java.util.regex.Pattern

trait Mocks {
  abstract sealed class ProtocolType {
    def failures(expected: List[ExpectedCall], received: List[ReceivedCall]): List[String]
  }
  case object inSequence extends ProtocolType {
    def failures(expected: List[ExpectedCall], received: List[ReceivedCall]): List[String] = {
      val unmatchedCalls = expected.zip(received).filter {case (e, r) => !e.expects(r)} map(_._1)
      if (unmatchedCalls.isEmpty)
        Nil
      else
        List("Unmatched protocol. Received:" + received.map {"\n  " + _.toString}.mkString(""))
    }
  }
  case object inAnyOrder extends ProtocolType {
    def failures(expected: List[ExpectedCall], received: List[ReceivedCall]): List[String] = {
      expected.filter {e: ExpectedCall => received.forall(!e.expects(_))}
        .map(_.toString + " should have been called")
    }
  }
  var protocol = new Protocol
  var recordingMode = false
  def expect[T](t: ProtocolType)(v: => T): Protocol = {
    protocol.protocolType = t
    recordingMode = true
    val result = v
    recordingMode = false
    protocol
  }
  def expect[T](v: => T): Protocol = expect[T](inAnyOrder)(v)
  def record = {
    if (recordingMode)
      protocol.addExpectedCall(methodName)
    else 
      protocol.call(methodName)
  }
  def methodName = {
    val message = (new Exception()).getStackTrace.dropWhile(!_.toString.contains("record"))(2).toString
    val matcherExp = Pattern.compile(".*\\.(.*\\(.*)").matcher(message)
    matcherExp.find
    matcherExp.group(1)
  }
  case class ExpectedCall(val method: String) {
    def expects(c: ReceivedCall) = (method == c.method)
    override def toString = method
  }
  case class ReceivedCall(val method: String){
    override def toString = method
  }
  class Protocol {
    var expectedCalls: List[ExpectedCall] = Nil
    var receivedCalls: List[ReceivedCall] = Nil
    var protocolType: ProtocolType = inAnyOrder 
    def addExpectedCall(m: String) = expectedCalls = expectedCalls ::: List(new ExpectedCall(methodName))
    def call(m: String) = receivedCalls = receivedCalls:::List(new ReceivedCall(m))
    def failures: List[String] = unexpectedCallsFailures:::unmatchedCallsFailures
    def unexpectedCalls = receivedCalls.filter {r: ReceivedCall => expectedCalls.forall(!_.expects(r))}
    def unexpectedCallsFailures = unexpectedCalls.map(_.toString + " should not have been called")  
    def unmatchedCallsFailures = protocolType.failures(expectedCalls, expectedReceivedCalls)
    def expectedReceivedCalls = receivedCalls.remove(unexpectedCalls.contains(_))
    def clear = { expectedCalls = Nil; receivedCalls = Nil }
    def isSpecified = !(expectedCalls).isEmpty
  }
}
