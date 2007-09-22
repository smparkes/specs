package scala.specs
import scala.util._
import scala.specs.matcher._
import scala.collection.mutable._
import scala.specs.runner._
import scala.specs.matcher.MatcherUtils._
import scala.specs.SpecUtils._
import scala.specs.specification._

/**
 * This class is the main class for declaring a new specification<br>
 * In the context of a specification, you can:<ul>
 * <li>declare nested specifications
 * <li>define systems under test
 * <li>specify examples and assertions</ul>
 * Usage: <code>object mySpec extends Specification</code>
 * <p>
 * A specification is "executed" when it is constructed, then the failures and errors can 
 * be collected with the corresponding methods
 *
 */
abstract class Specification extends Matchers with SpecificationStructure {
  /** adds a "before" function to the last sut being defined */
  def usingBefore(beforeFunction: () => Unit) = { suts.last.before = Some(beforeFunction) } 

  /** adds an "after" function to the last sut being defined */
  def usingAfter(afterFunction: () => Unit) = { suts.last.after = Some(afterFunction) }

  /** @return the failures of each sut */
  def failures = suts.flatMap {_.failures}

  /** @return the errors of each sut */
  def errors = suts.flatMap {_.errors}

  /** @return the total number of assertions for each sut */
  def assertionsNb = suts.foldLeft(0) {_ + _.assertionsNb}

  /** @return a description of this specification with all its suts (used for the ConsoleReporter) */
  def pretty = description + suts.foldLeft("") {_ + _.pretty(addSpace("\n"))}

  /** 
   * Convenience method: adds a new failure to the latest example<br>
   * Usage: <code>fail("this code should fail anyway")</code>
   */
  def fail(m: String) = lastExample.addFailure(FailureException(m))
}

/**
 * The <code>Sut</code> class represents a system under test<br>
 * It has:<ul>
 * <li>a description declaring what kind of system it is
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * It is also an <code>ExampleLifeCycle</code> so it can refine the passed cycle
 * <p>
 * In specifications, a Sut "should" or "can" provide some functionalities which are defined in <code>Examples</code><br>
 * A Sut is "executed" during its construction and failures and errors are collected from its examples
 */
case class Sut(description: String, cycle: ExampleLifeCycle) extends ExampleLifeCycle {
  /** default verb used to define the behaviour of the sut */
  var verb = "should"

  /** examples describing the sut behaviour */
  var examples = new Queue[Example]

  /** the before function will be invoked before each example */
  var before: Option[() => Unit] = None
  
  /** the after function will be invoked after each example */
  var after: Option[() => Unit] = None

  /** default way of defining the behaviour of a sut */
  def should(ex : Example) = {}

  /** alternately there may be no example given yet */
  def should(noExampleGiven: Unit) = {}

  /** Alias method to describe more advanced or optional behaviour. This will change the verb used to report the sut behavior */
  def can(ex : Example) = {verb = "can"}

  /** @return all examples failures */
  def failures = examples.flatMap {_.failures}

  /** @return all examples errors */
  def errors = examples.flatMap {_.errors}

  /** @return the total number of assertions for this sut */
  def assertionsNb = examples.foldLeft(0) {_ + _.assertionsNb}

  /** @return a description of this sut with all its examples (used for the ConsoleReporter) */
  def pretty(tab: String) = tab + description + " " + verb + " " + examples.foldLeft("") {_ + _.pretty(addSpace(tab))}

  /** calls the before method of the "parent" cycle, then the sut before method before an example if that method is defined. */
  override def beforeExample(ex: Example) = {
    cycle.beforeExample(ex)
    before.foreach {_.apply()}
  }

  /** forwards the call to the "parent" cycle */
  override def beforeTest(ex: Example) = { cycle.beforeTest(ex) }
  /** forwards the call to the "parent" cycle */
  override def afterTest(ex: Example) = { cycle.afterTest(ex) }

  /** calls the after method of the "parent" cycle, then the sut after method after an example if that method is defined. */
  override def afterExample(ex: Example) = { 
    cycle.afterExample(ex)
    after.foreach {_.apply()}
  }
}

/**
 * The <code>Example</code> class specifies one example of a system behaviour<br>
 * It has:<ul>
 * <li>a description explaining what is being done
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * <p>
 * Usage: <code>"this is an example" in { // code containing assertions }</code> or<br>
 * <code>"this is an example" >> { // code containing assertions }</code><br>
 * ">>" can be used instead of "in" if that word makes no sense in the specification
 * <p>
 * An example can also contain subexamples which are executed will evaluating the <code>in</code> method.
 * <p>
 * When assertions have been evaluated inside an example they register their failures and errors for later reporting 
 */
case class Example(description: String, cycle: ExampleLifeCycle) {
  /** failures created by Assert objects inside the <code>in<code> method */
  var thisFailures = new Queue[FailureException]

  /** errors created by Assert objects inside the <code>in<code> method */
  var thisErrors = new Queue[Throwable]

  /** number of <code>Assert</code> objects which refer to that Example */
  var assertionsNb = 0

  /**
   * utility variable used to keep track if an example is being defined in another one. In that case, it will be
   * created as a subexample
   */
  var isInsideDefinition = false

  /** sub-examples created inside the <code>in</code> method */
  var subExamples = new Queue[Example]

  /**
   * creates a new Example object and, in the process of doing so, evaluates the <code>test</code>
   * value which may contain assertions. Errors and failures are then attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * @return a new <code>Example</code>
   */
  def in (test: => Any): Example = {
    isInsideDefinition = true
    
    // try the "before" methods. If there is an exception, add an error and return the current example
    try { cycle.beforeExample(this) } catch {
      case t: Throwable => { 
        addError(t) 
        isInsideDefinition = false
        return this 
      }
    }
    // execute the <code>test</code> parameter. If it contains assertions they will be automatically executed
    try {
      cycle.beforeTest(this)
      test
      cycle.afterTest(this)
      } catch { 
      // failed assertions will launch a FailureException
      case f: FailureException => addFailure(f)
      case t: Throwable => {t.printStackTrace; addError(t)}
    }
    // try the "after" methods. If there is an exception, add an error and return the current example
    try { cycle.afterExample(this) } catch { case t: Throwable => addError(t) }
    isInsideDefinition = false
    this
  }
  /** alias for the <code>in</code> method */
  def >> (test: => Any) = in(test)
  
  /** creates and adds a new error from an exception t */
  def addError(t: Throwable) = thisErrors += t

  /** creates and adds a failure exception */
  def addFailure(failure: FailureException) = thisFailures += failure

  /** @return the failures of this example and its subexamples */
  def failures: Seq[FailureException] = thisFailures ++ subExamples.flatMap { _.failures }

  /** @return the errors of this example and its subexamples */
  def errors: Seq[Throwable] = thisErrors ++ subExamples.flatMap {_.errors}

  /** @return a user message with failures and messages, addSpaceed with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} + 
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
}

/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}
