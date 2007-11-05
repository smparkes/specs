package scala.specs
import scala.Products

object Sugar extends Sugar

/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions.<br>
 * The name reminds that it must be used with caution
 */
trait Sugar extends Products {
  
  /** alias for the value true. Allows to write <code> myObject.status mustBe ok </code>*/
  val ok = true
  
  /** alias for the value false. Allows to write <code> myObject.status mustBe ko </code>*/
  val ko = false


  /** 
   * This implicit definition allows to write short loops, ruby-style:<br>
   * <code> 3.times { i => doThis() } </code>. <br>
   * Warning: an integer variable i must be declared otherwise there will be a runtime exception
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) { 
    def times[T](f: (Int) => T)  = for (i <- 1 to n) f(i) 
  }
  
  /** 
   * This implicit definition allows to print any object to the console with:<br>
   * <code> myObject.pln </code> or <code> myObject.println </code>  
   */
  implicit def anyPrintable[T](a: T) = {
    new Object { 
      def println = Console.println(a)
      def pln = Console.println(a) 
    }
  }
  
  implicit def stringToMult(s: String) = {
    new Object {
      def mult(s: String, i: Int): String = {        
        if (i == 0) 
          ""
        else
          s + mult(s, i-1)
      }
      def * (i: Int): String = mult(s, i)
    }
  }
}

