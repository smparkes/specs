package scala.specs.specification
import scala.xml._
import scala.util._
import scala.specs.Sugar._

/**
 * This trait is experimental. It is supposed to help writing some literal specifications
 * using the xml capabilities of Scala.
 * Several "toy" specifications have been written using this style:<ul>
 * <li>bizSpecificationSpec
 * <li>calculatorBizSpec
 * <li>xmlRunnerSpec
 * </ul>
 */
trait LiteralSpecification  extends Specification with DataTables {
  
  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Object {def shh = ""}

  /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = { new Object {
    def as(p: Property[T]) = {p() = a; p.toString }
    def apply(p: Property[T]) = {p() = a; p.toString }
    def apply(f: T => Any)= {f(a); a.toString }
    def as(f: T => Any)= {f(a); a.toString }
  }}
  
  /**
   * This method allows to embbed a DataTable in a literal specification and display the results of its execution
   */
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable(table: ExecutableDataTable) = {
      forExample(desc) in {
        table.execute
        table.results
      }
      desc + "\n" + table.toString
    }
  }                
}



