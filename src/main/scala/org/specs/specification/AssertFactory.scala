package org.specs.specification

/**
 * This trait defines implicit definitions which are used to create Assert objects
 * and associate them with the latest defined example<br>
 * Usage: <code>
 * 2.1 must beCloseTo(2.0, .1)
 * </code>or<br><code>
 * theDouble(2.1) must beCloseTo(2.0, .1)
 * </code><p> 
 * Most of those definitions are declared because the implicit def mechanism doesn't seem to select
 * the most specific definition (to my understanding)<br>
 * The implicit def for Strings is even stranger since values are supposed to have String as a lower bound.
 * Then StringMatchers are expecting values with String as an upper bound so that effectively only String instances
 * will be used with the implicit def (only solution found to make it all work, to my current understanding again)
 */
trait AssertFactory {
  /** utility function to track the last example being currently defined, in order to be able to add assertions to it */ 
  protected[this] var example: Option[Example] 

  def addAssertion = {
    example match {
      case None => forExample("for Example").addAssertion
      case Some(e) => e.addAssertion
    }
  }
  def forExample(desc: String): Example

  /** implicit transformation of a String into an object supporting String matchers */
  implicit def theString[A >: String](value: =>A) = {
    addAssertion
    new AssertString[String](value.toString)
  }

  /** implicit transformation of an object into one supporting AnyMatcher matchers */
  implicit def theValue[A](value: =>A) = {
    addAssertion
    new Assert[A](value)
  }

  implicit def theBlock(value: =>Nothing) = {
    addAssertion
    new Assert[Nothing](value)
  }
  /** implicit transformation of an Iterable[String] into an object supporting IterableString matchers */
  implicit def toStringIterableAssert(value: =>Iterable[String]) = {
    addAssertion
    new AssertIterableString(value)
  }

  /** implicit transformation of an Iterable into an object supporting Iterable matchers */
  implicit def toIterableAssert[I <: AnyRef](value: =>Iterable[I]) = {
    addAssertion
    new AssertIterable[I](value)
  }
}
	