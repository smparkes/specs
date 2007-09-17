package scala.specs.mock
import scala.specs.matcher._
import java.util.regex.Pattern

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

