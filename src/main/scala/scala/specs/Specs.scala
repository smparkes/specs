package scala.specs

import scala.util._
import scala.collection.mutable._
import scala.specs.integration._

abstract class Specification extends Matchers {
  var description = createDescription(getClass.getName)
  var suts : List[Sut] = Nil
  private[this] var lastExample : Example = _ 
  
  def createDescription(s: String) = s.split("\\$").reverse.dropWhile(isInteger(_))(0).split("\\.").reverse(0)
  implicit def stringToExample(desc: String): Example = {
    lastExample = new Example(desc, suts.last)
    currentExamplesList += lastExample
    lastExample
  }
  def currentExamplesList = currentSut.examples.find { _.isInsideDefinition } match {
    case Some(parentExample) => parentExample.subExamples
    case None => currentSut.examples
  }
  def currentSut = suts.last
  implicit def stringToSut(desc: String) = { 
    suts = suts:::List(new Sut(desc))
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
  def usingBefore(beforeFunction: () => Unit) = { suts.last.before = Some(beforeFunction) } 
  def usingAfter(afterFunction: () => Unit) = { suts.last.after = Some(afterFunction) }
  implicit def compositeSpecification(description: String): Specification = { 
    this.description = description + " is specified by"
    this
  }
  def isSpecifiedBy(specifications: Specification*) = {
    specifications.flatMap(_.suts) foreach {sut: Sut => suts = sut::suts}
  }
  def fail(m: String) = lastExample.addFailure(FailureException(m))
  def failures = suts.flatMap {_.failures}
  def errors = suts.flatMap {_.errors}
  def assertionsNb = suts.foldLeft(0) {_ + _.assertionsNb}
}

case class Sut(description: String) {
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
}

case class Example(description: String, parent: Sut) {
  var thisFailures = new Queue[FailureException]
  var thisErrors = new Queue[Throwable]
  var assertionsNb = 0
  var isInsideDefinition = false
  var subExamples = new Queue[Example]
  def >> (test: => Any) = in(test)
  def in (test: => Any): Example = {
    isInsideDefinition = true
    try { parent.before.foreach {_.apply()} } catch {
      case t: Throwable => { 
        addError(t) 
        isInsideDefinition = false
        return this 
      }
    }
    try { test } catch { 
      case f: FailureException => addFailure(f)
      case t: Throwable => addError(t)
    }
    try { parent.after.foreach {_.apply()} } catch {
      case t: Throwable => addError(t) 
    }
    isInsideDefinition = false
    this
  }
  def addError(t: Throwable) = thisErrors += t
  def addFailure(failure: FailureException) = thisFailures += failure
  def failures: Seq[FailureException] = thisFailures ++ subExamples.flatMap { _.thisFailures }
  def errors = thisErrors ++ subExamples.flatMap {_.thisErrors}
}

class Assert[+T](value: => T, example: Example) extends Matchers {
  example.assertionsNb += 1
  
  def must[S >: T](m: Matcher[S]): Boolean =  {
    val (result, _, koMessage) = m(value) 
    result match {
      case false => throwFailure(koMessage)
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

  def mustThrow[E <: Throwable](errorType: E): Unit = {
    try { value } 
    catch {
      case x => if (errorType.getClass.isAssignableFrom(x.getClass)) 
                  return 
                else
                  { throwFailure(errorType + " should have been thrown. Got: " + x); return }
    }
    throwFailure(errorType + " should have been thrown")
  }
  def throwFailure[S >: T](failureMessage: String) = {
    val failure = FailureException(failureMessage) 
    failure.setStackTrace((failure.getStackTrace.dropWhile {x: StackTraceElement => matches("Assert")(x.toString)}).toArray)
    throw failure
  }
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
