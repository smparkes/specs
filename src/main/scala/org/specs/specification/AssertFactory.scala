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
  protected[this] def lastExample: Example 

  /** implicit transformation of a double into an object supporting numeric matchers */
  implicit def theDouble[A <: Double](value: => A) = {
    new Assert[A](value, lastExample)
  }

  /** implicit transformation of a Number into an object supporting numeric matchers */
  implicit def theNumber[A <: Number](value: => A) = {
    new Assert[A](value, lastExample)
  }

  /** implicit transformation of an Int into an object supporting numeric matchers */
  implicit def theInt[A <: Int](value: => A) = {
    new Assert[A](value, lastExample)
  }

  /** implicit transformation of a Boolean into an object supporting AnyMatcher matchers */
  implicit def theBoolean[A <: Boolean](value: => A) = {
    new Assert[A](value, lastExample)
  }
  
  /** implicit transformation of a Long into an object supporting AnyMatcher matchers */
  implicit def theLong[A <: Long](value: => A) = {
    new Assert[A](value, lastExample)
  }
    
  /** implicit transformation of a Float into an object supporting AnyMatcher matchers */
  implicit def theFloat[A <: Float](value: => A) = {
    new Assert[A](value, lastExample)
  }
      
  /** implicit transformation of a Byte into an object supporting AnyMatcher matchers */
  implicit def theByte[A <: Byte](value: => A) = {
    new Assert[A](value, lastExample)
  }
        
  /** implicit transformation of a Char into an object supporting AnyMatcher matchers */
  implicit def theChar[A <: Char](value: => A) = {
    new Assert[A](value, lastExample)
  }
          
  /** implicit transformation of a String into an object supporting String matchers */
  implicit def theString[A >: String](value: => A) = {
    new AssertString[String](value.toString, lastExample)
  }

  /** implicit transformation of an object into one supporting AnyMatcher matchers */
  implicit def theValue[A <: AnyRef](value: => A) = {
    new Assert[A](value, lastExample)
  }

  /** implicit transformation of an Iterable[String] into an object supporting IterableString matchers */
  implicit def toStringIterableAssert(value: Iterable[String]) = {
    new AssertIterableString(value, lastExample)
  }

  /** implicit transformation of an Iterable into an object supporting Iterable matchers */
  implicit def toIterableAssert[I <: AnyRef](value: Iterable[I]) = {
    new AssertIterable[I](value, lastExample)
  }
}
