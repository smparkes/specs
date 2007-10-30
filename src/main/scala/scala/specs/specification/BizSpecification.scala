package scala.specs.specification
import scala.xml._
import scala.util._
import scala.specs.Sugar._

trait BizSpecification  extends Specification {
  implicit def anyToShh(a: Any) = {new Object {def shh = ""}}
  implicit def anyToAs[T](a: T) = { new Object {
    def as(p: Property[T]) = {p() = a; p.toString }
    def apply(p: Property[T]) = {p() = a; p.toString }
    def apply(f: T => Any)= {f(a); a.toString }
    def as(f: T => Any)= {f(a); a.toString }
  }}
  implicit def toDataTable(header: (String, String)): TableStarter = { TableStarter(header.elements.toList) }
  implicit def toDataTable(header: (String, String, String)): TableStarter = { TableStarter(header.elements.toList) }
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable[T <: Any {def execute: String}](table: T) = {
      forExample(desc) in {
        table.execute
      }
      desc + "\n" + table.toString
    }
  }                
  case class TableStarter(header: List[String]) {
    def |[T1, T2](a: (T1, T2)) = DataTable2[T1, T2](header, List(a), false)
    def |>[T1, T2](a: (T1, T2)) = DataTable2[T1, T2](header, List(a), true)
    def |[T1, T2, T3](a: (T1, T2, T3)) = new DataTable3[T1, T2, T3](header, List(a), false)
    def |>[T1, T2, T3](a: (T1, T2, T3)) = new DataTable3[T1, T2, T3](header, List(a), true)
  }
  trait DataTable 
  case class DataTable2[T1, T2](header: List[String], values: List[(T1, T2)], shouldExecute: Boolean) extends DataTable {
    var function: Function2[T1, T2, Boolean] = _
    def | = this
    def |>(t: (T1, T2)) = new DataTable2[T1, T2](header, values ::: List(t), true)
    def |(t: (T1, T2)) = new DataTable2[T1, T2](header, values ::: List(t), shouldExecute)
    def |(f: Function2[T1, T2, Boolean]) = {
      function = f
      if (shouldExecute == true) execute(f)
      this
    }
    def execute: String = if (function != null) execute(function) else toString
    def execute(f: Function2[T1, T2, Boolean]): String = {
        var result = " " + header.mkString("|", "|", "|")
        var failure = false
        values foreach {v: (T1, T2) => { try {
          f(v._1, v._2)
          } catch {
            case e: FailureException => {failure = true; result += ("\nx" + v.toList.mkString("|", "|", "|") + " " + e.getMessage)}
          }} 
        }  
        if (failure) fail(result)
      toString
    }
    override def toString = {header.mkString("|", "|", "|\n") + values.map(_.mkString("|", "|", "|")).mkString("\n")}
  }  
  case class DataTable3[T1, T2, T3](header: List[String], values: List[(T1, T2, T3)], shouldExecute: Boolean) extends DataTable {
    var function: Function3[T1, T2, T3, Boolean] = _
    def | = this
    def |>(t: (T1, T2, T3)) = new DataTable3[T1, T2, T3](header, values ::: List(t), true)
    def |(t: (T1, T2, T3)) = new DataTable3[T1, T2, T3](header, values ::: List(t), shouldExecute)
    def |(f: Function3[T1, T2, T3, Boolean]) = {
      function = f
      if (shouldExecute == true) execute(f)
      this
    }
    def execute: String = if (function != null) execute(function) else toString
    def execute(f: Function3[T1, T2, T3, Boolean]): String = {
        var result = " " + header.mkString("|", "|", "|")
        var failure = false
        values foreach {v: (T1, T2, T3) => { try {
          f(v._1, v._2, v._3)
          } catch {
            case e: FailureException => {failure = true; result += ("\nx" + v.toList.mkString("|", "|", "|") + " " + e.getMessage)}
          }} 
        }  
        if (failure) fail(result)
      toString
    }
    override def toString = {header.mkString("|", "|", "|\n") + values.map(_.mkString("|", "|", "|")).mkString("\n")}
  }  
}



