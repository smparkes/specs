package scala.specs.matcher
import scala.specs.matcher.MatcherUtils._
import scala.specs.matcher.PatternMatchers._
import scala.specs.specification._

object AnyMatchers extends AnyMatchers
/**
 * The <code>AnyMatchers</code> trait provides matchers which are applicable to any scala Reference or Value
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
   * Matches is (a != b)
   */   
  def is_!=[T](a: T) = (is_==(a)).not 
  
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
   * Otherwise rethrow any other exception
   * Usage: value must throwA(new ExceptionType)
   * Advanced usage: value must throwA(new ExceptionType).like {case ExceptionType(m) => m.startsWith("bad")}
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
   * returns an Option with the expected exception if it satisfies function <code>f</code>
   * Rethrow the exception otherwise
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
   * creates a FailureException corresponding to a thrown exception.
   * Set the stacktrace of the Failure exception so that it starts with the code line where the original exception
   * was thrown
   */
  def throwFailure(e: Throwable, failureMessage: String) = {
    val failure = FailureException(failureMessage) 
    failure.setStackTrace((e.getStackTrace.dropWhile {x: StackTraceElement => matches("AnyMatchers")(x.toString)}).toArray)
    throw failure
  }

  /**
   * creates a FailureException corresponding to a thrown exception.
   * Set the stacktrace of the Failure exception so that:
   * -it drops 2 lines corresponding to place where the failure exception was created
   * -drop the lines corresponding to the object having created the exception (an Assert object)
   *  in order to start the stacktrace where the failure happened
   */
  def throwFailure(origin: Object, failureMessage: String) = {
    val failure = FailureException(failureMessage)
    failure.setStackTrace((failure.getStackTrace.drop(2).dropWhile {x: StackTraceElement => matches(origin.getClass.getName.split("\\.").last)(x.toString)}).toArray)
    throw failure
  }

}
