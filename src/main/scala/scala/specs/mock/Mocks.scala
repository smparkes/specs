package scala.specs.mock
import scala.specs.Sugar._
import scala.collection.mutable.Stack
import scala.util.ExtendedList._

/**
 * The <code>Protocol</code> class stores the expectations of mocks, alongside with the actual received calls
 * It can be exclusive or not: i.e. return an error in case of unexpected calls or not
 * A <code>protocol</code> has 3 functions:
 * -be defined by nested protocol definitions and expected calls
 * -store received calls
 * -return failures if there are unmatched calls
 */
class Protocol extends ProtocolTypes {
  /** actual calls received by mocks */
  var receivedCalls: List[ReceivedCall] = Nil

  /** if true, will check unexpected calls too */
  var exclusive = false

  /** 
   * protocol allDefinitions which are stacked during creation
   * but which will be nested after creation. Each protocol definition corresponds to 
   * a block of an "expect" declaration:
   * <code>expect(twoOf) {
   *    expect(inSequence) {
   *      mock.callMethod1
   *      mock.callMethod2
   *    }
   * }</code>
   */
  private var allDefinitions: Stack[ProtocolDef] = new Stack
  
  /** 
   * definition of the protocol 
   */
  def definition = allDefinitions.top

  /** 
   * takes a value <code>v</code>which will declare some mocks expectations
   * By default the protocol type is <code>inAnyOrder</code>
   */
  def expect(v: => Any): ProtocolDef = expect(inAnyOrder)(v)

  /** 
   * takes a value <code>v</code>which will declare some mocks expectations
   * The protocol type is specified by the parameter <code>t</code>
   */
  def expect(t: ProtocolType)(v: => Any): ProtocolDef = {
    // open a new protocol definition of type t with no expected calls
    allDefinitions.push(new ProtocolDef(t, Nil))
    // v is supposed to contain mock expectations
    v
    
    // if there are more than one protocol definition
    // the last protocol on the stack is "poped" and becomes a specified call for the previous protocol definition
    if (allDefinitions.size > 1) {
      val p = allDefinitions.pop
      allDefinitions.top.expect(p)
      p
    }
    else
      allDefinitions.top // else return the outermost protocol
  }

   /** 
    * add an expected method name to the current protocol definition 
    */
   def expectCall(methodName: String) = allDefinitions.top.expect(methodName)

   /** 
    * add a received call to the list of received calls 
    */
   def receiveCall(methodName: String) = receivedCalls = receivedCalls:::List(new ReceivedCall(methodName))

  /** 
   * returns an error message if the protocol definition 
   * has some expected calls which don't match the received calls
   */
  def failures: String = {
    val f = definition.failures(receivedCalls, exclusive)
    
    // if there are no failures that are the result of unmatched calls
    // and if the protocol is defined as exclusive, return a message with unexpected calls if there are some
    if (exclusive && f.isEmpty)
      receivedCalls.filter(!_.consumed).map(_.toString + " should not have been called").mkString("\n")
    else
      f
  }
  
  /** 
   * A <code>Protocol</code> is specified if there it contains one protocol definition 
   */
  def isSpecified = !allDefinitions.isEmpty

  /** 
   * Remove any previous protocol definition and received calls 
   */
  def clear = {
    allDefinitions = new Stack
    receivedCalls = Nil
  }
}

