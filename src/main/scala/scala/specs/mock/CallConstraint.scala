package scala.specs.mock

/**
 * This class is used to specify constraints on a sequence of expected calls
 * It must defines a </code>verifies</code> method which says if the constraint is verified 
 * for a number <code>n</code> of consumed calls.
 * It must also define a <code>stop</code> method to specify if a Specified call can stop consuming 
 * received calls
 * The <code>expectation</code> method returns a string describing what is expected by this constraint
 * for example: "at least 3 of:". That string is used to form meaningful error messages
 */
abstract sealed class CallConstraint { 
  def verifies(size: Int): Boolean
  /**
   * By default the received message consumption stops when the constraint is verified
   * excepted for the atLeast constraint which is "greedy" and tries to consume as many calls as possible 
   */
  def stop(n: Int): Boolean = verifies(n)
  def expectation: String
}
/**
 * This class expects exactly n received calls matching a specified call 
 */
case class exactlyN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size == n
  def expectation: String = n + " of:"
}

/**
 * This class expects at least n received calls matching a specified call 
 */
case class atLeastN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size >= n
  def expectation: String = "at least " + n + " of:"
  override def stop(n: Int): Boolean = false
}

/**
 * This class expects at most n received calls matching a specified call 
 */
case class atMostN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size <= n
  def expectation: String = "at most " + n + " of:"
}
