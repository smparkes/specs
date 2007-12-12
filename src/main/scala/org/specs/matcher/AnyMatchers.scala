package scala.specs.matcher

import org.specs.specification._
import org.specs.matcher.MatcherUtils.{q, matches}
import org.specs.matcher.PatternMatchers._

object AnyMatchers extends AnyMatchers
/**
 * The <code>AnyMatchers</code> trait provides matchers which are applicable to any scala reference or value
 */
trait AnyMatchers {

  /**
   * Matches if (a eq b)
   */   
  def be[T](a: T) = new Matcher[T](){
     def apply(b: =>T) = (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef], q(b) + " is the same as " + q(a), q(b) + " is not the same as " + q(a)) 
   } 
  def notBe[T](a: T) = be(a).not
  
  /**
   * Matches if (a == b)
   */   
  def is_==(a: Any) = new Matcher[Any](){ 
     def apply(b: =>Any) = ((a == b), q(b) + " is equal to " + q(a), q(b) + " is not equal to " + q(a))
   }

  /**
   * Alias of is_==
   */   
  def be_==(a: Any) = is_==(a)

  /**
   * Matches if (a neq b)
   */   
  def notEq[T](a: T) = be(a).not 

  /**
   * Matches if (a != b)
   */   
  def is_!=[T](a: T) = (is_==(a)).not 
  
  /**
   * Matches if (a != b)
   */   
  def be_!=[T](a: T) = (is_==(a)).not 

  /**
   * Matches if b is null
   */   
  def beNull[T] = new Matcher[T](){
     def apply(b: =>T) = (b == null, "the value is null", q(b) + " is not null") 
   }  

  /**
   * Matches if b is not null
   */   
  def notBeNull[T] = beNull[T].not

  /**
   * Matches if b is true
   */   
  def beTrue[T] = new Matcher[T](){
     def apply(b: =>T) = (b == true, q(b) + " is true", q(b) + " is false") 
   }  

  /**
   * Matches if b is false
   */   
  def beFalse[T] = new Matcher[T](){
     def apply(b: =>T) = (b == false, q(b) + " is false", q(b) + " is true") 
  }  
  
  /**
   * Matches if iterable.exists(_ == a)
   */   
  def beIn[T <: AnyRef](iterable: Iterable[T]) = new Matcher[T](){
     def apply(a: => T) = (iterable.exists(_ == a), q(a) + " is in " + q(iterable), q(a) + " is not in " + q(iterable))
  }

  /**
   * Matches if not(iterable.exists(_ == a))
   */   
  def notBeIn[T <: AnyRef](iterable: Iterable[T]) = beIn(iterable).not 

  /**
   * Matches if any object with an <code>isEmpty</code> method returns true: (Any {def isEmpty: Boolean}).isEmpty
   */   
  def beEmpty[S <: Any {def isEmpty: Boolean}] = new Matcher[S](){
     def apply(iterable: => S) = (iterable.isEmpty, iterable + " is empty", iterable + " is not empty")
  }

  /**
   * Matches if not(beEmpty(a))
   */   
  def notBeEmpty[S <: Any {def isEmpty: Boolean}] = beEmpty[S].not

  /**
   * Alias of notBeEmpty
   */   
  def isNotEmpty[S <: Any {def isEmpty: Boolean}] = notBeEmpty[S] 

  /**
   * Alias of beEmpty
   */   
  def isEmpty[S <: Any {def isEmpty: Boolean}] = beEmpty[S]

  /**
   * Matches if the function f returns true
   */   
  def verify[T](f: T => Boolean): Matcher[T] = new Matcher[T](){
     def apply(x: => T) = (f(x), x + " verifies the property", x + " doesn't verify the expected property")
  }
  
  /**
   * Matches if value is throwing an exception which is assignable from errorType.getClass
   * <br>Otherwise rethrow any other exception
   * <br>Usage: <code>value must throwA(new ExceptionType)</code>
   * <br>Advanced usage: <code>value must throwA(new ExceptionType).like {case ExceptionType(m) => m.startsWith("bad")}</code>
   */   
  def throwException[E <: Throwable](exception: E) = new Matcher[Any](){
     def apply(value: => Any) = { 
       (isThrown(value, exception, (e => exception.getClass.isAssignableFrom(e.getClass))).isDefined, exception + " was thrown", exception + " should have been thrown")
     }
     def like[E <: Throwable](f: =>(Any => Boolean)) = new Matcher[Any](){
       def apply(v: => Any) = {
         val thrown = isThrown(v, exception, (e => exception.getClass.isAssignableFrom(e.getClass)))
         if (!thrown.isDefined)
           (false, exception + " was thrown", exception + " should have been thrown")
         else 
           beLike(f)(thrown.get)
       }
     }
   } 
  /**
   * Alias for throwException
   */   
  def throwA[E <: Throwable](e: E) = throwException[E](e)

  /**
   * Alias for throwException
   */   
  def throwAn[E <: Throwable](e: E) = throwException[E](e)

  /**
   * Matches if the thrown exception is == to e
   */   
  def throwThis[E <: Throwable](exception: E) = new Matcher[Any](){
    def apply(value: => Any) = { 
        (isThrown(value, exception, (e => e == exception)).isDefined, exception + " was thrown", exception + " should have been thrown")
    }
  }
    
  /**
   * @returns an Option with the expected exception if it satisfies function <code>f</code>
   * <br>rethrows the exception otherwise
   */   
  private def isThrown[E <: Throwable](value: => Any, expected: E, f: (Throwable => Boolean)) = { 
    getException(value) match {
      case None => None
      case Some(e)  => if (f(e))
                         Some(e)
                       else
                         throwFailure(e, expected + " should have been thrown. Got: " + e)
    }
  }
  /** evaluates a value and return any exception that is thrown */
  private def getException[E <: Throwable](value: => Any): Option[Throwable] = {
    try { value } 
    catch { case e => return Some(e) }
    return None
  }

  /**
   * Creates a FailureException corresponding to a thrown exception.
   * <br>Sets the stacktrace of the <code>FailureException</code> so that it starts with the code line where the original exception
   * was thrown
   * @param e original exception 
   * @param failureMessage exception message 
   */
  def throwFailure(e: Throwable, failureMessage: String) = {
    val failure = FailureException(failureMessage) 
    failure.setStackTrace((e.getStackTrace.dropWhile {x: StackTraceElement => matches("AnyMatchers")(x.toString)}).toArray)
    throw failure
  }

  /**
   * Creates a FailureException corresponding to a thrown exception<br>	 
   * Sets the stacktrace of the Failure exception so that:
   * <ul><li>it drops 2 lines corresponding to place where the failure exception was created
   * <li>drop the lines corresponding to the object having created the exception (an Assert object)
   *  in order to start the stacktrace where the failure happened
   * </ul>
   * @param origin object which is supposed to thrown the <code>FailureException</code> 
   * @param failureMessage exception message 
   */
  def throwFailure(origin: Object, failureMessage: String) = {
    val failure = FailureException(failureMessage)
    failure.setStackTrace((failure.getStackTrace.drop(2).dropWhile {x: StackTraceElement => matches(origin.getClass.getName.split("\\.").last)(x.toString)}).toArray)
    throw failure
  }

}
