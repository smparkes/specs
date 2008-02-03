package org.specs.mock
import org.jmock.lib.legacy.ClassImposteriser
import org.jmock.lib.action._;
import org.jmock.api._
import org.jmock.internal.State;
import org.jmock.internal.StatePredicate;
import java.util.Collection;
import java.util.Iterator;
import org.hamcrest._;
import org.hamcrest.core._;
import org.hamcrest.core.AnyOf._;
import org.jmock._
import org.specs.specification._
import org.specs.util.Property
import org.specs.ExtendedThrowable._
import org.jmock.internal.matcher.MethodNameMatcher;
import org.specs.collection.JavaCollectionsConversion._

/** 
 * The JMocker trait is used to give access to the mocking functionalities of the JMock library 
 */
trait JMocker extends JMockerExampleLifeCycle with HamcrestMatchers with JMockActions {

  /**
   * the mock method is used to create a mock object
   * Usage:<pre>val mocked = mock(classOf[InterfaceToMock])</pre><br/>
   * Classes can be mocked too, but the ClassImposterizer trait has to be added to the extensions
   * @returns the mocked object
   */
  def mock[T](c: java.lang.Class[T]): T =  context.mock(c).asInstanceOf[T]

  /** 
   * mocks a class and give the resulting mock a name. jMock expects mocks of the same class to have different names
   * @returns the mocked object
   */
  def mock[T](c: java.lang.Class[T], name: String) =  context.mock(c, name).asInstanceOf[T]

  /** 
   * mocks a class and add expectations. Usage <code>willReturn(as(classOf[MyInterface]){m: MyInterface => one(m).method })<code>
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](c: java.lang.Class[T])(expects: Function1[T, Any]) = {
    val mocked = context.mock(c).asInstanceOf[T]
    (mocked, expects) 
  }

  /** 
   * mocks a class and add expectations, specifying the mock with a name
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](c: java.lang.Class[T], name: String)(expects: Function1[T, Any]) = {
    val mocked = context.mock(c, name).asInstanceOf[T]
    (mocked, expects) 
  }

  /** 
   * mocks a class several times and add expectations for each mock. Usage <code>willReturn(as(classOf[MyInterface])
   *   {m1: MyInterface => one(m1).method },
   *   {m2: MyInterface => one(m2).method },
   * )<code>
   * However, it is shorter to use <code>willReturnIterable:
   * one(workspace).projects willReturnIterable(classOf[Project], 
           {p: Project => one(p).name willReturn "p1" },
           {p: Project => one(p).name willReturn "p2" })<code>
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](c: java.lang.Class[T], expects: Function1[T, Any]*) = {
    expects.toList.zipWithIndex map { x =>
      val (block, i) = x
      (context.mock(c, c.getName + "_" + i).asInstanceOf[T], block)
    }
  }

  /** 
   * the code block being evaluated should contain some expectations being added to the expectations variable
   * Then the expectations are "freezed" by calling the checking method on context and the actual system behaviour can
   * be exercised
   */
  def expect(block: => Any) = {
    val result = block;
    context.checking(expectations);
    result
  }

  /** 
   * Adds call constraint method to integers to express the exactly, atLeast, atMost
   */
  implicit def intToCallConstraint(i: Int) = new IntCallConstraint(i)
  class IntCallConstraint(i: Int) {
    def of[T](a: T) = exactly(i).of(a)
    def atLeastOf[T](a: T) = atLeast(i).of(a)
    def atMostOf[T](a: T) = atMost(i).of(a)
  }

  /** 
   * Adds call constraint method to a range, to express the between constraint
   */
  implicit def RangeToCallConstraint(r: Range) = new RangeCallConstraint(r)
  class RangeCallConstraint(r: Range) {
    def of[T](a: T) = between(r.start, r.end).of(a)
  }
  
  /** expecting exactly one call to the mock */
  def one[T](m: T) = expectations.one(m)

  /** expecting exactly count calls to the mock */
  def exactly(count: Int) = expectations.exactly(count)
    
  /** expecting atLeast count calls to the mock */
  def atLeast(count: Int) = expectations.atLeast(count)
    
  /** expecting between minCount and maxCount (inclusive) calls to the mock */
  def between(minCount: Int, maxCount: Int) = expectations.between(minCount, maxCount)

  /** expecting at Most calls to the mock */
  def atMost(count: Int) = expectations.atMost(count)
    
  /** allowing any calls to mocks */
  def allowing[T](mockObjectMatcher: Matcher[T]) = expectations.allowing(mockObjectMatcher)
    
  /** allowing any calls to a mock with method names like the passed parameter, returning default values */
  def allowingMatch[T](mock: T, methodName: String) = expectations.allowing(new IsSame(mock)).method(withName(methodName))

  /** allowing any calls to any mock with method names like the passed parameter, returning default values */
  def allowingMatch(methodName: String) = expectations.allowing(anything).method(withName(methodName))

  /** allowing any calls to the mock */
  def allowing[T](mockObject: T) = expectations.allowing(mockObject)
    
  /** ignoring any calls to the mock, returning default values */
  def ignoring[T](mockObject: T) = expectations.ignoring(mockObject)
    
  /** ignoring any calls to the mock, returning default values */
  def ignoring[T](mockObjectMatcher: Matcher[T]) = expectations.ignoring(mockObjectMatcher)
    
  /** ignoring any calls to the mock with method names like the passed parameter, returning default values */
  def ignoringMatch(methodName: String) = expectations.ignoring(anything).method(withName(methodName))

  /** ignoring any calls to a mock with method names like the passed parameter, returning default values */
  def ignoringMatch[T](mock: T, methodName: String) = expectations.ignoring(new IsSame(mock)).method(withName(methodName))
 
  /** forbidding any calls to the mock */
  def never[T](mockObject: T) = expectations.never(mockObject)

  /** constraining the latest call expectation with a Hamcrest matcher */
  def `with`[T](matcher: Matcher[T]) = expectations.`with`(matcher)

  /** shortcut for expectations.`with`(new IsAnything[Int]) */
  def anyInt = any(classOf[Int])

  /** shortcut for expectations.`with`(new IsAnything[Long]) */
  def anyLong = any(classOf[java.lang.Long])

  /** shortcut for expectations.`with`(new IsAnything[Short]) */
  def anyShort = any(classOf[Short])

  /** shortcut for expectations.`with`(new IsAnything[Boolean]) */
  def anyBoolean = any(classOf[Boolean])

  /** shortcut for expectations.`with`(new IsAnything[Float]) */
  def anyFloat = any(classOf[Float])

  /** shortcut for expectations.`with`(new IsAnything[Double]) */
  def anyDouble = any(classOf[Double])

  /** shortcut for expectations.`with`(new IsAnything[Char]) */
  def anyChar = any(classOf[Char])

  /** shortcut for expectations.`with`(new IsAnything[Byte]) */
  def anyByte = any(classOf[Byte])

  /** shortcut for expectations.`with`(new IsAnything[String]) */
  def anyString = any(classOf[String]) 

  /** shortcut for expectations.`with`(new IsAnything[T]) */
  def any[T](t: java.lang.Class[T]) = expectations.`with`(anything[T])

  /** shortcut for expectations.`with`(new IsInstanceOf[T]) */
  def a[T](t: java.lang.Class[T]) = {expectations.`with`(new IsInstanceOf(t)); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsInstanceOf[T]) */
  def an[T](t: java.lang.Class[T]) = a(t)

  /** shortcut for expectations.`with`(new IsNull[T]) */
  def aNull[T](t: java.lang.Class[T]) = {expectations.`with`(new IsNull[T]); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsNot(IsNull[T])) */
  def aNonNull[T](t: java.lang.Class[T]) = {expectations.`with`(new IsNot(new IsNull[T])); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsEqual[T](value)) */
  def equal[T](value: T)  = {expectations.`with`(new IsEqual[T](value)); value}

  /** shortcut for expectations.`with`(new IsSame[T](value)) */
  def same[T](value: T)  = {expectations.`with`(new IsSame[T](value)); value}
  
  /** Adapter class to use specs matchers as Hamcrest matchers */
  class HamcrestMatcherAdapter[T](m: org.specs.matcher.Matcher[T]) extends org.hamcrest.TypeSafeMatcher[T] {
     var resultMessage: String = ""
     def matchesSafely(item: T) = {
       val result = m.apply(item)
       if (result._1)
         resultMessage = result._2
       else
         resultMessage = result._3
       result._1
     }
     def describeTo(description: Description ) = {
       new org.hamcrest.StringDescription(new StringBuffer(resultMessage))
     }
  }

  /** @returns a specs matcher adapted into Hamcrest matcher */
  implicit def will[T](m: org.specs.matcher.Matcher[T]): T = {
    expectations.`with`(new HamcrestMatcherAdapter[T](m))
    null.asInstanceOf[T]
  }
  
  /** this method is used to avoid the use of the reserved Scala keyword 'with'. This is also consistent with the way to specify returned values */
  def will[T](m: org.hamcrest.Matcher[T]): T = {
    expectations.`with`(m)
    null.asInstanceOf[T]
  }

  /** allow the return value of the method to be more precisely specified with a JMock action, like a returnValue action */
  implicit def toAction[T](v: T) = new JMockAction(v)

  /** 
   * This class allows a block of code to be followed by some actions like returning a value or throwing an exception
   * When we wish to return a mock and define expectations at the same time:
   * <code>willReturn(classOf[MyInterface]) { m1: MyInterface =>
   *    one(m1).method }
   * <code>
   * It is necessary to first create the mock, set it as a returned object through a ReturnValueAction
   * then to trigger the block containing the expectations for that mock
   */
  class JMockAction[T](v: T) {

    private[this] def wrap[T](collection: java.util.Collection[T]) = collection.toArray

    /** sets a value to be returned by the mock */
    def willReturn(result: T) = expectations.will(new ReturnValueAction(result))

    /** sets an action which will return a mock of type Class[T] and being specified by a function triggering expectations */
    def willReturn(c: java.lang.Class[T])(f: Function1[T, Any]): Unit = {
      val mocked = context.mock(c).asInstanceOf[T]
      expectations.will(new ReturnValueAction(mocked))
      f(mocked)
    }
    
    /** 
     * sets an action which will return a mock of type Class[T] and being specified by a function triggering expectations.
     * It is supposed to be used in conjunction with the <code>as</code> method which mocks an object and creates a function
     * defining the mock expectations
     */
    def willReturn(result: (T, Function1[T, Any])) = {
      expectations.will(new ReturnValueAction(result._1))
      result._2.apply(result._1)
    }

    def willReturnEach(values: T*) = {
      will(new ActionSequence((values map { x: T => returnValue(x) }).toArray))
    }

   /** 
    * will return a list of mocks from the same class several times and add expectations for each mock. 
    * Usage <code>willReturnIterable(as(classOf[MyInterface])
    *   {m1: MyInterface => one(m1).method },
    *   {m2: MyInterface => one(m2).method },
    * )<code>
    * However, it is shorter to use <code>willReturnIterable:
    * one(workspace).projects willReturnIterable(classOf[Project], 
            {p: Project => one(p).name willReturn "p1" },
            {p: Project => one(p).name willReturn "p2" })<code>
    */
    def willReturnIterable[S, T <: Iterable[S]](c: java.lang.Class[S], results: Function1[S, Any]*): Unit = {
      willReturn(results.toList.zipWithIndex map { x =>
        val (block, i) = x
        (context.mock(c, c.getName + "_" + i).asInstanceOf[S], block)
      })
    }
 
   /** 
    * will return a list of values of type S, and containing some associated blocks to set expectations for those values which may be mocks 
    * Usage <code>willReturnIterable(as(classOf[MyInterface]){m1: MyInterface => one(m1).method },
    *   as(classOf[MyInterface]){m2: MyInterface => one(m2).method })
    * <code>
    */
    def willReturnIterable[S, T <: Iterable[S]](results: (S, Function1[S, Any])*): Unit = willReturn(results.toList)

   /** 
    * will return a list of values of type S, and containing some associated blocks to set expectations for those values which may be mocks 
    * Usage <code>willReturnIterable(List(as(classOf[MyInterface]){m1: MyInterface => one(m1).method },
    *   as(classOf[MyInterface]){m2: MyInterface => one(m2).method }))
    * <code>
    */
    def willReturn[S, T <: Iterable[S]](results: List[(S, Function1[S, Any])]): Unit = {
      expectations.will(new ReturnValueAction(results.map(_._1).toList))
      results foreach { result =>
        result._2.apply(result._1)
      }
    }

    /** sets an exception to be thrown by the mock */
    def willThrow[X <: Throwable](t: X) = expectations.will(new ThrowAction(t))

    /** set up a JMock action to be executed */
    def will(action: Action) = expectations.will(action)
  }
  
  /** set up a jMock action to be executed */
  def will(action: Action) = expectations.will(action)
    
  /** @returns a state with name <code>name<code>, creating a new one if necessary */
  def state(name: String) = context.states(name)

  /** specifies that a call can only occur following the specific condition on a state */
  def when(predicate: StatePredicate) = expectations.when(predicate)
    
  /** specifies that after a call, a State object will be in a specific state */
  def then(state: State) = expectations.then(state)

  /** allows any block of expectations to be followed by state actions and expectations */
  implicit def afterCall(v: Any) = new StateConstraint(v)

  /** this class allows any block of expectations to be followed by state actions and expectations */
  class StateConstraint(expectation: Any) {
    def set(state: State) = expectations.then(state)
    def when(predicate: StatePredicate) = expectations.when(predicate)
  }

  /** adds a constraint to the expectations to declare that an expected call must happen in sequence */
  def inSequence(sequence: Sequence) = expectations.inSequence(sequence)

  /** this class allows an expectation to declare that another expectation should follow */
  implicit def after(v: =>Any) = new InSequenceThen(v)

  /** this class allows an expectation to declare that another expectation should follow */
  class InSequenceThen(firstExpectation: =>Any) {
    val sequence = {
      val s = context.sequence("s")
      firstExpectation; inSequence(s)
      s
    }  
    def then(otherExpectation: Any) = {
      otherExpectation
      inSequence(sequence)
      this
    }
  }
  
}

/**
 * This trait provide methods to build Hamcrest matchers. It actually contains only 2 methods since all Hamcrest matchers can
 * be created by static methods in the Hamcrest library classes
 */
trait HamcrestMatchers {
  /** shortcut for new IsAnything[T] */
  def anything[T] = new IsAnything[T]

  /** @returns a MethodNameMatcher to use in conjunction with allowing(mock) */
  def withName(nameRegex: String) = new MethodNameMatcher(nameRegex)
}

/**
 * This trait provide methods to build jMock actions
 */
trait JMockActions {
  /** action returning a value */
  def returnValue[T](result: T) = new ReturnValueAction(result)

  /** action throwing an exception of type T*/
  def throwEx[T <: Throwable](t: T) = new ThrowAction(t)
    
  /** action executing several other actions */
  def doAll(actions: Action*) = new DoAllAction(actions.toArray)
    
  /** @returns a sequence of actions where the first action is executed after the first call, the second action
   * after the second call, and so on */
  def onConsecutiveCalls(actions: Action*) = new ActionSequence(actions.toArray)
}

/** 
 * This trait defines when a jMock context will be created and expectations checked.
 * The context and expectations are always created at the beginning of an Example, then checked just after the test has been executed. Executing a test can also trigger an ExpectationError (from the jMock library). This error will be transformed into a FailureException
 */
trait JMockerExampleLifeCycle extends ExampleLifeCycle with JMockerContext {

  /** 
   * before any example, a new context and new exptecations should be created
   */
  override def beforeExample(ex: Example) = {
    super.beforeExample(ex)
    context = createMockery
    expectations = new Expectations()
  }

  /** 
   * An expectation error may be thrown during the execution of a test
   * In that case, it is transformed to a failure exception
   */
  override def executeTest(t: => Any) = {
    try { t } catch { case e: ExpectationError => throw createFailure(e) }
  }

  /** 
   * After a test the context is verified. If some more expectations are not met an ExpectationError is thrown
   * In that case, it is transformed to a failure exception
   */
  override def afterTest(ex: Example) = {
    try {
      context.assertIsSatisfied
    } catch {
      case e: ExpectationError => throw createFailure(e)
    }
    super.afterTest(ex)
  }
                         
  private[this] def createFailure(e: ExpectationError) = FailureException(e.toString)

}

/** 
 * This trait contains the jMock Mockery object and current expectations
 */
trait JMockerContext extends Imposterizer {
  /** the context holds the mocks, the expectations so is able to get the calls and check them */
  var context = createMockery

  /** call expectations with optional constraints on the number of calls, on parameters, return values,... */
  var expectations = new Expectations()
}


/** 
 * This trait allows to mock classes instead of interfaces only. This will require the cglib and objenesis libraries on the path. 
 */
trait ClassMocker extends Imposterizer {
  override def createMockery = new Mockery() { setImposteriser(ClassImposteriser.INSTANCE) }
}

/** 
 * Abstract trait for creating a mocking context. By default, only allows to mock interfaces 
 */
trait Imposterizer {
  def createMockery = new Mockery
}