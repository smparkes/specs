package scala.specs.mock
import scala.specs.matcher._
import scala.specs.Sugar._
import scala.collection.mutable.Stack
import java.util.regex.Pattern
import scala.util.ExtendedList._

/**
 * This class
 */
class Protocol extends ProtocolTypes {
  var receivedCalls: List[ReceivedCall] = Nil
  var exclusive = false
  private var protocolDefs: Stack[ProtocolDef] = new Stack
  def expect(v:  => Any): ProtocolDef = expect(inAnyOrder)(v)
  def expect(t: ProtocolType)(v: => Any): ProtocolDef = {
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
  def failures: String = {
    val f = protocolDef.failures(receivedCalls, exclusive)
    if (exclusive && f.isEmpty)
      receivedCalls.filter(!_.consumed).map(_.toString + " should not have been called").mkString("\n")
    else
      f
  }
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
  def consume(received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall])
  def toString: String
  def passes : Boolean = false
  var repetition : CallConstraint = atLeastN(0)
}
case class ExpectedCall(val method: String) extends SpecifiedCall {
  var callNumbers = 0
  override def passes : Boolean = repetition.verifies(callNumbers)
  def consume(received: List[ReceivedCall]) = {
    var found = false
    received takeWhile(r => {
    // ("trying " + method + " with " + r + " consumed " + r.consumed + " found " + found).pln
      if (r.method == method && !found && !r.consumed){
        callNumbers += 1
        r.consumedBy = Some(this)
        if (repetition.stop(callNumbers))
          found = true
        else
          found = false
        !found
      }
      else
        true
    })
    (List(this), received)
  }
  override def toString = method
}
case class ReceivedCall(val method: String) {
  var consumedBy: Option[SpecifiedCall] = None
  override def toString = method
  def consumed = (consumedBy != None)
}
case class ProtocolDef(val protocolType: ProtocolType, var expectedCalls: List[SpecifiedCall]) extends SpecifiedCall with ProtocolTypes {
  def expect(m: String) = expectedCalls = expectedCalls:::List(ExpectedCall(m))
  def expect(p: ProtocolDef) = expectedCalls = expectedCalls:::List(p)

  def failures(rs: List[ReceivedCall], exclusive: Boolean): String = protocolType.failures(expectedCalls, rs, exclusive)
  def consume(received: List[ReceivedCall]) = {
    protocolType.consume(expectedCalls, received)
  }
  override def passes : Boolean = expectedCalls.forall(_.passes)
  override def toString = protocolType.expectedDefs(expectedCalls)
}

trait Mocker extends ProtocolTypes with ExampleLifeCycle with MockMatchers {
  val protocol = new Protocol
  private var expectingMode = 0
  def expect(v: => Any): Protocol = expect(inAnyOrder, nonExclusively)(v)
  def expect(t: ProtocolType)(v: => Any): Protocol = expect(t, nonExclusively)(v)
  def expect(t: ProtocolType, e: Exclusivity)(v: => Any): Protocol = {
    if (expectingMode == 0) protocol.clear
    expectingMode += 1
    if (e == exclusively) { 
      protocol.exclusive = true
    } 
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
    expect(t, nonExclusively)(v)
  }

}

