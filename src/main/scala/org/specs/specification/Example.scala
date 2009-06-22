/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
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
import org.specs.execute._

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
case class Example(var exampleDescription: ExampleDescription, cycle: ExampleLifeCycle) extends TreeNode with Tagged with DefaultResults {
  def this(desc: String, cycle: ExampleLifeCycle) = this(ExampleDescription(desc), cycle)

  def description = exampleDescription.toString

  /** number of <code>Assert</code> objects which refer to that Example */
  protected[specification] var expectationsNumber = 0

  /** @return the number of expectations, executing the example if necessary */
  def expectationsNb = { execute; expectationsNumber }

  /** increment the number of expectations in this example */
  def addExpectation = { expectationsNumber += 1; this }

  /** sub-examples created inside the <code>in</code> method */
  private[specification] var subExs = new Queue[Example]

  /** add a new sub-example to this example */
  def addExample(e: Example) = {
    addChild(e)
    subExs += e
  }
  def createExample(desc: String, lifeCycle: ExampleLifeCycle) = {
    val ex = new Example(ExampleDescription(desc), lifeCycle)
    addExample(ex)
    ex
  }
  def copyExecutionResults(other: Example) = {
    this.hardCopyResults(other)
    this.subExs = other.subExs
    this.expectationsNumber = other.expectationsNumber
    other.childNodes.foreach(this.addChild(_))
    this.execution.executed = true
  }

  /** @return the subexamples, executing the example if necessary */
  def subExamples = { execute; subExs }
  /** @return this example if it doesn't have subexamples or return the subexamples */
  def allExamples: List[Example] = {
    if (subExs.isEmpty)
      List(this)
    else
      subExs.flatMap(_.allExamples).toList
  }
  def doTest[T](expectations: => T) = cycle.executeTest(this, expectations)

  /** encapsulates the expectations to execute */
  var execution = new ExampleExecution(this, () => ())

  /**
   * creates a new Example object and store as an ExampleExecution object the expectations to be executed.
   * This <code>expectations</code> parameter is a block of code which may contain expectations with matchers.
   * Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, expectations number, subexamples
   * @return a new <code>Example</code>
   */
  def in(expectations: =>Any): this.type = {
    setExecution(expectations)
    this
  }
  def in(example: =>Example): Unit = setExecution(example)
  
  private def setExecution(a: =>Any): Unit = {
    execution = new ExampleExecution(this, () => {
      cycle.setCurrentExample(Some(this))
      a
      cycle.setCurrentExample(None)
    })
    if (cycle.isSequential)
      execute
  }
  /** alias for the <code>in</code> method */
  def >>(expectations: =>Any) = in(expectations)
  def >>(example: =>Example) = in(example)

  /** execute the example, checking the expectations. */
  def execute = if (!execution.executed) cycle.executeExample(this)

  def before = {}
  def after = {}
  def execute(t: => Any): Any = {
    val executed = t
    skipIfNoExpectations()
    executed
  }
  protected def skipIfNoExpectations() = {
    if (this.expectationsNumber == 0 && 
          this.subExs.isEmpty && //this.thisSkipped.isEmpty && this.thisFailures.isEmpty && this.thisErrors.isEmpty && 
          Configuration.config.examplesWithoutExpectationsMustBePending)
      throw new SkippedException("PENDING: not yet implemented").removeTracesAsFarAsNameMatches("(specification.Example|LiterateSpecification)")
  }

  /** @return the failures of this example and its subexamples, executing the example if necessary */
  override def failures: List[FailureException] = { ownFailures ++ subExamples.flatMap { _.failures } }
  /** @return the failures of this example, executing the example if necessary */
  def ownFailures: List[FailureException] = { execute; thisFailures.toList }

  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  override def skipped: List[SkippedException] = { ownSkipped ++ subExamples.flatMap { _.skipped } }
  /** @return the skipped messages for this example, executing the example if necessary  */
  def ownSkipped: List[SkippedException] = { execute; thisSkipped.toList }

  /** @return the errors of this example, executing the example if necessary  */
  override def errors: List[Throwable] = { ownErrors ++ subExamples.flatMap {_.errors} }
  def ownErrors: List[Throwable] = { execute; thisErrors.toList }

  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} +
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
  /** @return the example description */
  override def toString = description.toString

  def executeThis = execution.execute
  /** reset in order to be able to run the example again */
  def resetForExecution: this.type = {
    execution.resetForExecution
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear
    subExs.foreach(_.resetForExecution)
    this
  }
  /** @return the example for a given Activation path */
  def getExample(path: TreePath): Option[Example] = {
    path match {
      case TreePath(Nil) => Some(this)
      case TreePath(i :: rest) if !this.subExamples.isEmpty => this.subExamples(i).getExample(TreePath(rest))
      case _ => None
    }
  }
}
/**
 * Description of the example
 */
case class ExampleDescription(desc: String, toXhtml: Node) {
  override def toString = desc
  def format: String = toXhtml.toString
}
object ExampleDescription {
  def apply(desc: String): ExampleDescription = ExampleDescription(desc, <ex>{desc}</ex>)
}
/**
 * This class encapsulates the execution of an example
 */
class ExampleExecution(example: Example, val expectations: () => Any) {
  /** function containing the expectations to be run */
  private var toRun: () => Any = () => {
    if (example.isAccepted) {
      execution()
      while (!example.cycle.until) execution()
    } else
      example.addSkipped(new SkippedException("not tagged for execution"))
  }

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[specification] var executed = false
  val execution = () => {
    var failed = false
    // try the "before" methods. If there is an exception, add an error and return the current example
    try { example.cycle.beforeExample(example) } catch {
      case t: Throwable => {
        example.addError(t)
        failed = true
      }
    }
    // execute the <code>expectations</code> parameter. If it contains expectations with matchers they will be automatically executed
    try {
      if (!failed) {
        example.cycle.beforeTest(example)
        example.cycle.executeTest(example, expectations())
        example.cycle.afterTest(example)
      }
    } catch {
      // failed expectations will launch a FailureException
      // skipped expectations will launch a SkippedException
      case f: FailureException => example.addFailure(f)
      case s: SkippedException => example.addSkipped(s)
      case t: Throwable => example.addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try {
        if (!failed)
          example.cycle.afterExample(example)
      } catch { case t: Throwable => example.addError(t) }
      example
  }
  /** execute the example, setting a flag to make sure that it is only executed once */
  def execute = {
    if (!executed) {
      toRun()
      executed = true
    }
  }
  def resetForExecution = executed = false
}