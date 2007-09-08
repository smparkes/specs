package scala.specs

import scala.util._
import scala.specs.matcher._
import scala.collection.mutable._
import scala.specs.integration._
import scala.specs.matcher.MatcherUtils._
import scala.specs.SpecUtils._

abstract class Specification extends Matchers with SpecificationBuilder {
  var description = createDescription(getClass.getName)
  def createDescription(s: String) = s.split("\\$").reverse.dropWhile(isInteger(_))(0).split("\\.").reverse(0)
  def usingBefore(beforeFunction: () => Unit) = { suts.last.before = Some(beforeFunction) } 
  def usingAfter(afterFunction: () => Unit) = { suts.last.after = Some(afterFunction) }
  def fail(m: String) = lastExample.addFailure(FailureException(m))
  def failures = suts.flatMap {_.failures}
  def errors = suts.flatMap {_.errors}
  def assertionsNb = suts.foldLeft(0) {_ + _.assertionsNb}
  def pretty = description + suts.foldLeft("") {_ + _.pretty(indent("\n"))}
  implicit def compositeSpecification(d: String): Specification = { description = d; this }
  def areSpecifiedBy(specifications: Specification*) = {
    this.description += " are specified by"
    subSpecifications = subSpecifications:::specifications.toList
  }
  def isSpecifiedBy(specifications: Specification*) = {
    this.description += " is specified by"
    subSpecifications = subSpecifications:::specifications.toList
  }
  private def addSuts(others: Seq[Sut]) = suts = suts:::others.toList
}

case class Sut(description: String, cycle: ExampleLifeCycle) extends ExampleLifeCycle {
  var verb = "should"
  var before: Option[() => Unit] = None
  var after: Option[() => Unit] = None
  var examples = new Queue[Example]
  def can(ex : Example) = {verb = "can"}
  def should(ex : Example) = {}
  def should(noExampleGiven : Unit) = {}
  def failures = examples.flatMap {_.failures}
  def errors = examples.flatMap {_.errors}
  def assertionsNb = examples.foldLeft(0) {_ + _.assertionsNb}
  def pretty(tab: String) = tab + description + " " + verb + " " + examples.foldLeft("") {_ + _.pretty(indent(tab))}
  override def beforeExample(ex: Example) = {
    cycle.beforeExample(ex)
    before.foreach {_.apply()}
  }
  override def beforeTest(ex: Example) = { cycle.beforeTest(ex) }
  override def afterTest(ex: Example) = { cycle.afterTest(ex) }
  override def afterExample(ex: Example) = { 
    cycle.afterExample(ex)
    after.foreach {_.apply()}
  }
}

case class Example(description: String, cycle: ExampleLifeCycle) {
  var thisFailures = new Queue[FailureException]
  var thisErrors = new Queue[Throwable]
  var assertionsNb = 0
  var isInsideDefinition = false
  var subExamples = new Queue[Example]
  def >> (test: => Any) = in(test)
  def in (test: => Any): Example = {
    isInsideDefinition = true
    try { cycle.beforeExample(this) } catch {
      case t: Throwable => { 
        addError(t) 
        isInsideDefinition = false
        return this 
      }
    }
    try {
      cycle.beforeTest(this)
      test
      cycle.afterTest(this)
      } catch { 
      case f: FailureException => addFailure(f)
      case t: Throwable => {t.printStackTrace; addError(t)}
    }
    try { cycle.afterExample(this) } catch { case t: Throwable => addError(t) }
    isInsideDefinition = false
    this
  }
  
  def addError(t: Throwable) = thisErrors += t
  def addFailure(failure: FailureException) = thisFailures += failure
  def failures: Seq[FailureException] = thisFailures ++ subExamples.flatMap { _.failures }
  def errors: Seq[Throwable] = thisErrors ++ subExamples.flatMap {_.errors}
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + indent(tab) + _.message} + 
                                                      errors.foldLeft("") {_ + indent(tab) + _.getMessage}
}

class Assert[+T](value: => T, example: Example) extends Matchers {
  example.assertionsNb += 1
  
  def must[S >: T](m: => AbstractMatcher[S]): Boolean =  {
    val (result, _, koMessage) = m.apply(value) 
    result match {
      case false => throwFailure(this, koMessage)
      case _ => true
    }
  }
  def verify(f: T => Boolean) = must(function(f))
  def verifies(function: T => Boolean) = verify(function)

  def mustBe[S >: T](otherValue: S) = must(be(otherValue)) 
  def mustNotBe[S >: T](otherValue: S) = must(notEq(otherValue)) 
  def mustEq[S >: T](otherValue: S) = must(be(otherValue))
  def mustNotEq[S >: T](otherValue: S) = mustNotBe(otherValue)
  def must_!=[S >: T](otherValue: S) = must(is_!=(otherValue))
  def must_==[S >: T](otherValue: S) = must(is_==(otherValue))
  def mustEqual[S >: T](otherValue: S) = must(is_==(otherValue))
}
case class FailureException(message: String) extends RuntimeException(message)

class AssertString(value: => String, example: Example) extends Assert[String](value, example) {
  def mustMatch(a: String) = must(beMatching(a))
  def mustNotMatch(a: String) = must(not(beMatching(a)))
  def must_==/(a: String) = must(equalIgnoreCase(a))
  def must_!=/(a: String) = must(notEqualIgnoreCase(a))
}
class AssertIterable[I <: AnyRef](value: Iterable[I], example: Example) extends Assert[Iterable[I]](value, example) {
  def mustExist(function: I => Boolean) = must(exist {x:I => function(x)})
  def mustNotExist(function: I => Boolean) = must(notExist{x:I => function(x)})
  def mustContain(elem: I) = must(contain(elem))
  def mustNotContain(elem: I) = must(notContain(elem))
}
class AssertIterableString(value: Iterable[String], example: Example) extends AssertIterable[String](value, example) {
  def mustHaveMatch(elem: String) = must(existMatch(elem))
  def mustNotHaveMatch(elem: String) = must(notExistMatch(elem))
  def mustExistMatch(pattern: String) = must(existMatch(pattern))
  def mustNotExistMatch(pattern: String) = must(notExistMatch(pattern))
}
trait SpecificationBuilder extends ExampleLifeCycle {
  var subSpecifications: List[Specification] = Nil
  var suts : List[Sut] = Nil
  protected[this] var lastExample : Example = _ 
  implicit def stringToExample(desc: String): Example = {
    lastExample = new Example(desc, currentSut)
    currentExamplesList += lastExample
    lastExample
  }
  def currentExamplesList = currentSut.examples.find { _.isInsideDefinition } match {
    case Some(parentExample) => parentExample.subExamples
    case None => currentSut.examples
  }
  def currentSut = suts.last
  implicit def stringToSut(desc: String) = { 
    suts = suts:::List(new Sut(desc, this))
    suts.last
  }
  implicit def toAssertDouble[A <: Double](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toAssertNumber[A <: Number](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toAssertInt[A <: Int](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toAssertUnit[A <: Unit](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toAssertBoolean[A <: Boolean](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toAssertString[A >: String](value: => A) = {
    new AssertString(value.asInstanceOf[String], lastExample)
  }
  implicit def toAssert[A <: AnyRef](value: => A) = {
    new Assert[A](value, lastExample)
  }
  implicit def toStringIterableAssert(value: Iterable[String]) = {
    new AssertIterableString(value, lastExample)
  }
  implicit def toIterableAssert[I <: AnyRef](value: Iterable[I]) = {
      new AssertIterable[I](value, lastExample)
  }
}
trait ExampleLifeCycle {
  def beforeExample(ex: Example) = {} 
  def beforeTest(ex: Example)= {}
  def afterTest(ex: Example) = {}
  def afterExample(ex: Example) = {}
}
object SpecUtils {
  def indent(s: String) = s + "  "
}
