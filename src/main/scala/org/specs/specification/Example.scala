package org.specs.specification

import org.specs.util.ExtendedString._
import org.specs.util._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.ExtendedThrowable._
import scala.reflect.Manifest
/**
 * The <code>Example</code> class specifies one example of a system behaviour<br>
 * It has:<ul>
 * <li>a description explaining what is being done
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * <p>
 * Usage: <code>"this is an example" in { // code containing expectations }</code> or<br>
 * <code>"this is an example" >> { // code containing expectations }</code><br>
 * ">>" can be used instead of "in" if that word makes no sense in the specification
 * <p>
 * An example can also contain subexamples which are executed will evaluating the <code>in</code> method.
 * <p>
 * When expectations have been evaluated inside an example they register their failures and errors for later reporting 
 */
case class ExampleWithContext[S](val context: SystemContext[S], var exampleDesc: ExampleDescription, cyc: ExampleLifeCycle) extends Example(exampleDesc, cyc) {
  override def createExample(desc: String, lifeCycle: ExampleLifeCycle) = {
    val ex = new ExampleWithContext(context, ExampleDescription(desc), lifeCycle)
    addExample(ex)
    ex
  }
  override def before = {
    context.init
    context.before(context.system)
  }
  override def after = {
    context.after(context.system)
  }
  override def execute(t: => Any) = {
    val test = t
    test match {
      case function: Function1[S, Any] => function(context.system)
      case function: Function2[S, Context, Any] => function(context.system, context)
      case _ => t
    }
  }

} 
case class Example(var exampleDescription: ExampleDescription, cycle: ExampleLifeCycle) extends Tagged with HasResults {
  def this(desc: String, cycle: ExampleLifeCycle) = this(ExampleDescription(desc), cycle)

  def description = exampleDescription.toString

  /** function containing the test to be run */
  private[this] var toRun: () => Any = () => ()

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[this] var executed = false
  
  /** failures created by Assert objects inside the <code>in<code> method */
  var thisFailures = new Queue[FailureException]

  /** skipped created by Assert objects inside the <code>in<code> method */
  var thisSkipped = new Queue[SkippedException]

  /** errors created by Assert objects inside the <code>in<code> method */
  var thisErrors = new Queue[Throwable]

  /** number of <code>Assert</code> objects which refer to that Example */
  private[this] var expectationsNumber = 0

  /** @return the number of expectations, executing the example if necessary */
  def expectationsNb = { execute; expectationsNumber }

  /** increment the number of expectations in this example */
  def addExpectation = { expectationsNumber += 1; this }

  /** sub-examples created inside the <code>in</code> method */
  private[this] var subExs = new Queue[Example]

  /** add a new sub-example to this example */
  def addExample(e: Example) = subExs += e
  def createExample(desc: String, lifeCycle: ExampleLifeCycle) = {
    val ex = new Example(ExampleDescription(desc), lifeCycle)
    addExample(ex)
    ex
  }

  /** @return the subexamples, executing the example if necessary */
  def subExamples = {execute; subExs}
  
  /** alias for the <code>in</code> method */
  def >>[T](test: => T) = in(test)
  def doTest[T](test: => T) = cycle.executeTest(this, test)
  /**
   * creates a new Example object and store as a function the test to be executed. This <code>test</code>
   * is a value which may contain expectations. Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, expectations number, subexamples
   * @return a new <code>Example</code>
   */
  def in[T](test: => T): Example = {
    val execution = () => {
      var failed = false
      // try the "before" methods. If there is an exception, add an error and return the current example
      try { cycle.beforeExample(this) } catch {
        case t: Throwable => { 
          addError(t) 
          failed = true
        }
      }
      // execute the <code>test</code> parameter. If it contains expectations they will be automatically executed
      try {
        if (!failed) {
          cycle.beforeTest(this)
          cycle.executeTest(this, test)
          cycle.afterTest(this)
        }
      } catch { 
        // failed expectations will launch a FailureException
        // skipped expectations will launch a SkippedException
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case t: Throwable => addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try { 
        if (!failed) 
          cycle.afterExample(this) 
      } catch { case t: Throwable => addError(t) }
      this
    }
    toRun = () => {
      if (isAccepted) {
        execution()
        while (!cycle.until) execution()
      } else
        addSkipped(new SkippedException("not tagged for execution"))
    }
    if (cycle.isSequential)
      execute
    this
  }
  
  def before = {}
  def after = {}
  def execute(t: => Any) = t

  /** execute the example, setting a flag to make sure that it is only executed once */
  private[this] def execute = {
    if (!executed){
      toRun()
      executed = true
    }
  }
  
  /** creates and adds a new error from an exception t */
  def addError(t: Throwable) = thisErrors += t

  /** creates and adds a failure exception */
  def addFailure(failure: FailureException) = thisFailures += failure

  /** creates and adds a skipped exception */
  def addSkipped(skip: SkippedException) = thisSkipped += skip

  /** @return the failures of this example and its subexamples, executing the example if necessary */
  def failures: Seq[FailureException] = { execute; thisFailures ++ subExamples.flatMap { _.failures } }

  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  def skipped: Seq[SkippedException] = { execute; thisSkipped ++ subExamples.flatMap { _.skipped } }

  /** @return the errors of this example and its subexamples, executing the example if necessary  */
  def errors: Seq[Throwable] = { execute; thisErrors ++ subExamples.flatMap {_.errors} }

  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} + 
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
  /** @return the example description */
  override def toString = description.toString
  
  /** reset in order to be able to run the example again */
  def resetForExecution: this.type = {
    executed = false
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear
    subExs.foreach(_.resetForExecution)
    this
  }
}
case class ExampleDescription(desc: String) {
  override def toString = desc
  def format: String = desc.toString
}
