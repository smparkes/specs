package scala.specs.mock
import scala.specs.matcher._
import java.util.regex.Pattern

/**
 * This trait allows to define mocked method and to specify how they can be called
 * It is used in 3 steps: 
 * -first create a mock object which will override the methods that you want to mock
 *  val mock = new MyClass {
 *    override def method { record }
 *  }   
 * -implement the mocked methods with the <code>Mocker.record</code> method
 * -declare expectations in your specification:
 *  expect { mock.method }
 * -every expectation will be automatically checked at the end of the example (see the implementation of the
 *  <code>ExampleLifeCycle</code>trait)
 */
trait Mocker extends ProtocolTypes with ExampleLifeCycle with MockMatchers {
  /** protocol storing mocks expectations */
  val protocol = new Protocol

  /** 
   * this variable is used to distinguish uses of the <code>record</code> method. Either during
   * expectation definitions or during actual calls
   */
  private var expectingMode = 0

  /** 
   * expects some methods to be called on mocks. Any call to the <code>record</code> method
   * during the evaluation of v will create a new expectation
   * Usage expect(twoOf, exclusively) { mock.method }
   */
  def expect(t: ProtocolType, e: Exclusivity)(v: => Any): Protocol = {
    if (expectingMode == 0) protocol.clear
    expectingMode += 1
    if (e == exclusively) 
      protocol.exclusive = true
    protocol.expect(t)(v); 
    expectingMode -= 1 
    protocol
  }

  /** 
   * default expect method: inAnyOrder, nonExclusively
   */
  def expect(v: => Any): Protocol = expect(inAnyOrder, nonExclusively)(v)

  /** 
   * next default expect method: any protocol type "t", nonExclusively
   */
  def expect(t: ProtocolType)(v: => Any): Protocol = expect(t, nonExclusively)(v)
  
  /** 
   * record a method call. If the expecting mode is > 0, that is, if we are inside an expect { } block
   * then add an expectation with the mocked method name. Otherwise, this is a regular call which is recorded as
   * a received call
   */
  def record: Unit = {
    if (expectingMode > 0) 
      protocol.expectCall(methodName)
    else
      protocol.receiveCall(methodName)
  }

  /** 
   * record a method call and return a specific value.
   * Usage: <code>val mock = new MyClass { override def method: Int = recordAndReturn(1) }</code> 
   * will return 1 on every call to the method <code>method</code>
   */
  def recordAndReturn[T](v: T): T = {
    record
    v
  }
  
  /** 
   * convenience method to add an assertion to check method parameters during calls
   * Usage: <code>def createMock(f: Movie => Unit) = new MovieRater { override def register(m: Movie) =  record(f(m)) }</code>
   * then <code> val mock = createMock((m: Movie) => {m.name must notBe(null)}) </code>
   */
  def record[T](v: T): T = {
    record
    v
  }

  /**
   * get the name of the recorded method by throwing an exception and parsing the stacktrace
   */
  private def methodName = {
    val message = (new Exception()).getStackTrace.dropWhile(!_.toString.contains("record")).dropWhile(_.toString.contains("record"))(0).toString
    val matcherExp = Pattern.compile(".*\\.(.*\\(.*)").matcher(message)
    matcherExp.find
    matcherExp.group(1)
  }

  /**
   * clear the protocol before and after each example to start with new expectations
   */
  override def beforeExample(ex: Example) = {
    protocol.clear
  } 
  override def afterExample(ex: Example) = {
    protocol.clear
  }

  /**
   * if some expectations have been made during the test, check them
   */
  override def afterTest(ex: Example) = {
    if (protocol.isSpecified) 
      (new Assert[Protocol](protocol, ex)) must beMet
    protocol.clear
  }
  
  /**
   * syntactic sugar allowing to write <code>
   * expect {
	 *   twoOf {  // instead of expect(twoOf) {
   *     mock.call
	 *   } 
   * }
   */
  implicit def protocolTypeToProtocolDef(t: ProtocolType)(v: => Any) = {
    expect(t, nonExclusively)(v)
  }
}

