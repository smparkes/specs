package scala.specs.specification
import scala.xml._
import scala.util._
import scala.specs.Sugar._

trait BizSpecification  extends Specification with DataTables {
  implicit def anyToShh(a: Any) = {new Object {def shh = ""}}
  implicit def anyToAs[T](a: T) = { new Object {
    def as(p: Property[T]) = {p() = a; p.toString }
    def apply(p: Property[T]) = {p() = a; p.toString }
    def apply(f: T => Any)= {f(a); a.toString }
    def as(f: T => Any)= {f(a); a.toString }
  }}
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



