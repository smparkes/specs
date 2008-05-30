package org.specs
import org.specs.util._
import org.specs.util.ExtendedString._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.ExtendedThrowable._

/**
 * This trait can be reused in any test based framework to access Matchers functionalities
 */
trait SpecsMatchers extends Matchers with AssertFactory with DefaultAssertionListener

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
abstract class Specification extends Matchers with AssertFactory with Application with Contexts { outer =>
  /** nested reporter so that a specification is executable on the console */
  private val reporter = new ConsoleRunner(this)

  /** A specification has a main method to be executable and print its result on a Console */
  override def main(args: Array[String]) = reporter.main(args)
  
  /**
   * Alternate constructor with the name of the specification
   */
  def this(n: String) = { this(); name = n; description = n; this }

  /**
   * Alternate constructor with subspecifications
   */
  def this(subspecs: Specification*) = { this(); subSpecifications = subspecs.toList; this }

  /** 
   * Syntactic sugar for examples sharing between systems under test.<p>
   * Usage: <code>  
   *   "A stack below full capacity" should {
   *    behave like "A non-empty stack below full capacity" 
   *    ...
   * </code>
   * In this example we suppose that there is a system under test with the same name previously defined.
   * Otherwise, an Exception would be thrown, causing the specification failure at construction time.
   */
  object behave {
    def like(other: Sut): Example = {
      val behaveLike = "behave like " + other.description.uncapitalize in {}
      other.examples.foreach(behaveLike.addExample(_))
      behaveLike
    }
    def like(sutName: String): Example = outer.suts.find(_.description == sutName) match {
      case Some(sut) => this.like(sut)
      case None => throw new Exception(q(sutName) + " is not specified in " + outer.name)
    }
  }

  /** @return the failures of each sut */
  def failures: List[FailureException] = subSpecifications.flatMap{_.failures} ::: suts.flatMap {_.failures}

  /** @return the skipped of each sut */
  def skipped: List[SkippedException] = subSpecifications.flatMap{_.skipped} ::: suts.flatMap {_.skipped}

  /** @return the errors of each sut */
  def errors: List[Throwable] = subSpecifications.flatMap{_.errors} ::: suts.flatMap {_.errors}

  /** @return the total number of assertions for each sut */
  def assertionsNb: Int = subSpecifications.foldLeft(0) {_ + _.assertionsNb} + suts.foldLeft(0) {_ + _.assertionsNb}

  /** @return a description of this specification with all its suts (used for the ConsoleReporter) */
  def pretty = description + suts.foldLeft("") {_ + _.pretty(addSpace("\n"))}

  /** 
   * Convenience method: adds a new failure to the latest example<br>
   * Usage: <code>fail("this code should fail anyway")</code>
   */
  def fail(m: String) = FailureException(m).rethrowFrom(this)

  /** 
   * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
   * Usage: <code>fail</code>
   */
  def fail: Nothing = fail("failure")

  /** 
   * Convenience method: adds a new skippedException to the latest example<br>
   * Usage: <code>skip("this example should be skipped")</code>
   */
  def skip(m: String) = SkippedException(m).rethrowFrom(this)
  
  /** @return true if there are failures or errors */
  def isFailing: Boolean = !this.failures.isEmpty || !this.errors.isEmpty
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
case class Sut(description: String, var cycle: org.specs.specification.ExampleLifeCycle) extends ExampleLifeCycle {
  /** default verb used to define the behaviour of the sut */
  var verb = ""

  /** 
   * instead of using several examples, a whole text with embedded assertions can be used to
   * specify the Sut
   */
  var literalDescription: Option[String] = None

  /** examples describing the sut behaviour */
  var examples = new Queue[Example]
  def addExample(e: Example) = examples += e
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None
  
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  
  /** a predicate which will decide if an example must be re-executed */
  var untilPredicate: Option[() => Boolean] = None

  var skippedSut: Option[Throwable] = None
  var failedSut: Option[String] = None

  /** default way of defining the behaviour of a sut */
  def should(ex: =>Example) = {
    verb = "should"
    specifyExamples(ex)
  }
  /** Alias method to describe more advanced or optional behaviour. This will change the verb used to report the sut behavior */
  def can(ex: =>Example) = { verb = "can"; specifyExamples(ex) }

  private def specifyExamples(ex: =>Example) = {
    try { ex } catch {
      case e: SkippedException => skippedSut = Some(e)
      case FailureException(m) => failedSut = Some(m)
    }
    this
  }

  /** alternately there may be no example given yet */
  def should(noExampleGiven: Unit) = { verb = "should"; this }
  
  /** specifies the system with a literal description and embedded assertions */
  def is(e: => Elem)= {
      verb = "specifies"
      literalDescription = Some(e.text)
  }

  /** @return all examples failures */
  def failures = examples.flatMap {_.failures}

  /** @return all examples skipped messages */
  def skipped = examples.flatMap {_.skipped}

  /** @return all examples errors */
  def errors = examples.flatMap {_.errors}

  /** @return the total number of assertions for this sut */
  def assertionsNb = examples.foldLeft(0) {_ + _.assertionsNb}

  /** @return a description of this sut with all its examples (used for the ConsoleReporter) */
  def pretty(tab: String) = tab + description + " " + verb + " " + examples.foldLeft("") {_ + _.pretty(addSpace(tab))}
  
  /** forwards the call to the "parent" cycle */
  override def until = { cycle.until && this.untilPredicate.getOrElse(() => true)() }

  /** calls the before method of the "parent" cycle, then the sut before method before an example if that method is defined. */
  override def beforeExample(ex: Example) = {
    cycle.beforeExample(ex)
    before.foreach {_.apply()}
  }

  /** forwards the call to the "parent" cycle */
  override def beforeTest(ex: Example) = { cycle.beforeTest(ex) }

  /** forwards the call to the "parent" cycle */
  override def executeTest(ex: Example, t: =>Any) = { 
    cycle.executeTest(ex, t) 
  }

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
case class Example(description: String, cycle: org.specs.specification.ExampleLifeCycle) {

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
  private[this] var assertionsNumber = 0

  /** @return the number of assertions, executing the example if necessary */
  def assertionsNb = { execute; assertionsNumber }

  /** increment the number of assertions in this example */
  def addAssertion = { assertionsNumber += 1 }

  /** sub-examples created inside the <code>in</code> method */
  private[this] var subExs = new Queue[Example]

  /** add a new sub-example to this example */
  def addExample(e: Example) = subExs += e

  /** @return the subexamples, executing the example if necessary */
  def subExamples = {execute; subExs}

  /**
   * creates a new Example object and store as a function the test to be executed. This <code>test</code>
   * is a value which may contain assertions. Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, assertions number, subexamples
   * @return a new <code>Example</code>
   */
  def in (test: => Any): Example = {
    val execution = () => {
      var failed = false
      // try the "before" methods. If there is an exception, add an error and return the current example
      try { cycle.beforeExample(this) } catch {
        case t: Throwable => { 
          addError(t) 
          failed = true
        }
      }
      // execute the <code>test</code> parameter. If it contains assertions they will be automatically executed
      try {
        if (!failed) {
          cycle.beforeTest(this)
          cycle.executeTest(this, test)
          cycle.afterTest(this)
        }
      } catch { 
        // failed assertions will launch a FailureException
        // skipped assertions will launch a SkippedException
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case t: Throwable => addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try { if (!failed) cycle.afterExample(this) } catch { case t: Throwable => addError(t) }
      this
    }
    toRun = () => {
      execution()
      while (!cycle.until) execution()
    }
    if (cycle.isSequential)
      execute
    this
  }
  
  /** execute the example, setting a flag to make sure that it is only executed once */
  private[this] def execute = {
    if (!executed){
      toRun()
      executed = true
    }
  }
  
  /** alias for the <code>in</code> method */
  def >> (test: => Any) = in(test)
  
  /** creates and adds a new error from an exception t */
  def addError(t: Throwable) = thisErrors += t

  /** creates and adds a failure exception */
  def addFailure(failure: FailureException) = thisFailures += failure

  /** creates and adds a skipped exception */
  def addSkipped(skip: SkippedException) = thisSkipped += skip

  /** @return the failures of this example and its subexamples, executing the example if necessary */
  def failures: Seq[FailureException] = {execute; thisFailures ++ subExamples.flatMap { _.failures }}

  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  def skipped: Seq[SkippedException] = {execute; thisSkipped ++ subExamples.flatMap { _.skipped }}

  /** @return the errors of this example and its subexamples, executing the example if necessary  */
  def errors: Seq[Throwable] = {execute; thisErrors ++ subExamples.flatMap {_.errors}}

  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} + 
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
  /** @return the example description */
  override def toString = description
}

/**
 * This traits adds before / after capabilities to specifications, so that a context can be defined for
 * each system under test being specified.
 */
trait Contexts extends SpecificationStructure {
  /** 
   * @deprecated
   * adds a "before" function to the last sut being defined 
   */
  def usingBefore(actions: () => Any) = currentSut.before = Some(actions)

  /** adds a "before" function to the last sut being defined */
  def doBefore(actions: =>Any) = currentSut.before = Some(() => actions)

  /** 
   * @deprecated
   * adds an "after" function to the last sut being defined 
   */
  def usingAfter(actions: () => Any) = currentSut.after = Some(actions)

  /** 
   * adds an "after" function to the last sut being defined 
   */
  def doAfter(actions: =>Any) = currentSut.after = Some(() => actions)

  /** 
   * repeats examples according to a predicate 
   */
  def until(predicate: =>Boolean) = currentSut.untilPredicate = Some(() => predicate)

  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  implicit def toSutActions(actions: =>Unit) = SutActions(() => actions)

  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  case class SutActions(actions: () =>Unit) {
    def before = usingBefore(actions)
    def after = usingAfter(actions)
  }

  /** 
   * Syntactic sugar to create pass a new context before creating a sut.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ...
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sut.
   */
  implicit def toContext(s: String) = ToContext(s) 

  /** 
   * Syntactic sugar to create pass a new context before creating a sut.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ...
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sut.
   */
  case class ToContext(s: String) {
    def ->-(context: Context): Sut = {
      if (context == null) throw new NullPointerException("the context is null")
      val sut = specify(s)
      usingBefore(context.beforeActions)
      usingAfter(context.afterActions)
      until(context.predicate())
      sut
    } 
  }

  /** Factory method to create contexts with before only actions */
  def beforeContext(actions: => Any) = new Context { before(actions) }

  /** Factory method to create contexts with before only actions and an until predicate */
  def beforeContext(actions: => Any, predicate: =>Boolean) = new Context { before(actions); until(predicate()) }

  /** Factory method to create contexts with after only actions */
  def afterContext(actions: => Any) = new Context { after(actions) }

  /** Factory method to create contexts with after only actions and an until predicate */
  def afterContext(actions: => Any, predicate: =>Boolean) = new Context { after(actions); until(predicate()) }

  /** Factory method to create contexts with after only actions */
  def context(b: => Any, a: =>Any) = new Context { before(b); after(a) }

  /** Factory method to create contexts with after only actions and an until predicate */
  def context(b: => Any, a: =>Any, predicate: =>Boolean) = new Context { before(b); after(a); until(predicate()) }
}
/** 
 * Case class holding before and after functions to be set on a system under test
 */
case class Context {
  var beforeActions: () => Any = () => () 
  var afterActions: () => Any = () => ()
  var predicate: () => Boolean = () => true
  def before(actions: =>Any) = { beforeActions = () => actions; this }
  def after(actions: =>Any) = { afterActions = () => actions; this }
  def until(predicate: =>Boolean) = { this.predicate = () => predicate; this }
}
/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}
