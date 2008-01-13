package org.specs.mock
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.jmock.lib.legacy.ClassImposteriser
import org.jmock.lib.action._;
import org.jmock.api._
import org.jmock.internal.State;
import org.jmock.internal.StatePredicate;
import java.util._;
import org.hamcrest._;
import org.hamcrest.core._;
import org.jmock._
import org.specs.runner._

object jmockSpecRunner extends ConsoleRunner(jmockSpec)
object jmockSpec extends Specification with ButtonAndLightJMock {
  "A class" should {
    "be mockable using JMock" in {
      expect {
        one(light).on
      }
      button.push
    } 
  }
}
trait ButtonAndLightJMock extends ButtonAndLight with JMocker with ClassMocker {
  val light = mock(classOf[Light])
  val button = Button(light)
}
trait JMocker extends ExampleLifeCycle with Imposterizer {
  var context = getMockery
  var e = new Expectations
  def mock[T](c: java.lang.Class[T]) =  context.mock(c).asInstanceOf[T]
  def expect(v: => Any) = {v; context.checking(e)}

  override def afterExample(ex: Example) = {
    context = new Mockery() { setImposteriser(ClassImposteriser.INSTANCE) }
    e = new Expectations
  }

  override def afterTest(ex: Example) = {
    context.assertIsSatisfied
  }

  def one[T](m: T) = e.one(m)

  def exactly(count: Int) = e.exactly(count)
    
  def atLeast(count: Int) = e.atLeast(count)
    
  def between(minCount: Int, maxCount: Int) = e.between(minCount, maxCount)

  def atMost(count: Int) = e.atMost(count)
    
  def allowing[T](mockObjectMatcher: Matcher[T]) = e.allowing(mockObjectMatcher)
    
  def allowing[T](mockObject: T) = e.allowing(mockObject)
    
  def ignoring[T](mockObject: T) = e.ignoring(mockObject)
    
  def ignoring[T](mockObjectMatcher: Matcher[T]) = e.ignoring(mockObjectMatcher)
    
  def never[T](mockObject: T) = e.never(mockObject)

  def withA[T](matcher: Matcher[T]) = e.`with`(matcher)

  def withA(matcher: Matcher[Boolean]) = e.`with`(matcher)
    
  def withA(matcher: Matcher[Byte]) = e.`with`(matcher)

  def withA(matcher: Matcher[Short]) = e.`with`(matcher)

  def withA(matcher: Matcher[Int]) = e.`with`(matcher)
  
  def withA(matcher: Matcher[Long]) = e.`with`(matcher)
    
  def withA(matcher: Matcher[Float]) = e.`with`(matcher)

  def withA(matcher: Matcher[Double]) = e.`with`(matcher)
    
  def will(action: Action) = e.will(action)

  def equal[T](value: T)  = new IsEqual(value)

  def same[T](value: T)  = new IsSame(value)
  
  def any[T](t: java.lang.Class[T]) = new IsAnything
    
  def anything[T]() = new IsAnything

  def a[T](t: java.lang.Class[T]) = new IsInstanceOf(t)
    
  def an[T](t: java.lang.Class[T]) = new IsInstanceOf(t)
    
  def aNull[T](t: java.lang.Class[T]) = new IsNull[T]
    
  def aNonNull[T](t: java.lang.Class[T]) = new IsNot(new IsNull[T])
    
  def returnValue[T](result: T) = new ReturnValueAction(result)

  def throwException[T <: Throwable](t: T) = new ThrowAction(t)
    
  def returnIterator[T](collection: Collection[T]) = new ReturnIteratorAction(collection)
    
  def returnIterator[T](collection: T*) = new ReturnIteratorAction(collection.toArray)
    
  def doAll(actions: Action*) = new DoAllAction(actions.toArray)
    
  def onConsecutiveCalls(actions: Action*) = new ActionSequence(actions.toArray)
    
  def when(predicate: StatePredicate) = e.when(predicate)
    
  def then(state: State) = e.then(state)

  def inSequence(sequence: Sequence) = e.inSequence(sequence)

}
trait ClassMocker extends Imposterizer {
  override def getMockery = new Mockery() { setImposteriser(ClassImposteriser.INSTANCE) }
}
trait Imposterizer {
  def getMockery = new Mockery
}