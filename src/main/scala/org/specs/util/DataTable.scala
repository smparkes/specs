package org.specs.util
import org.specs.Products._
import scala._
import org.specs.specification.FailureException

/**
 * The Datatables trait provides implicit methods to start table headers 
 * and DataRows.<p>
 */
trait DataTables {
  /**
   * @returns a table header which first column is the string <code>a</code> 
   */
  implicit def toTableHeader(a: String) = TableHeader(List(a))

  /**
   * @returns a table row whose type will be <code>T</code> for each element and
   * which starts with <code>a</code> as the first element 
   */
  implicit def toDataRow[T](a: T) = DataRow1(a)
}

/**
 * The TableHeader case class models the header of a data table which should be a list of strings
 * A header can be created using the | operator a separator between strings<pre>
 * "a" | "b" | "c = a + b"|
 * </pre>
 * A header can be closed using the | method which will return the TableHeader object<p>
 * A header can be followed by data rows which only requirement is to have a <code>def header_=(t: TableHeader)</code> function
 */
case class TableHeader(h: List[String]) {
  /**
   * Adds a new column to the header
   * @returns the extended header
   */
  def |(s: String) = TableHeader(h:::List(s))

  /**
  * Used to close the header
  * @returns the header
  */
  def | = this

  /**
   * Accepts any object on which a header can be set. Sets the header on that object and returns it
   * @returns the header-accepting object (usually a DataRow object)
   */
  def |[T <: Any {def header_=(t: TableHeader)}](d: T) = {d.header_=(this); d}

  /**
   * Accepts any object on which a header can be set and which is executable. 
   * Sets the header on the object, marks it as "should be executed" and returns it.
   * @returns the header-accepting object (usually a DataRow object), ready to execute
   */
  def |>[T <: Any {def header_=(t: TableHeader); def shouldExecute_=(b: Boolean)}](d: T) = {d.header_=(this); d.shouldExecute_=(true); d}

  /**
   * @returns the header as string: |"a" | "b" | "c = a + b"|
   */
   override def toString = h.mkString("|", "|", "|")
}

/**
 * The DataRow and DataTable classes are created with the following Ruby script:
 * N=20
 * def types(n)
 *   (1..n).map{|i| "T#{i-1}"}.join(", ")
 * end
 * def values_decl(n)
 * (1..n).map{|i| "v#{i-1}: T#{i-1}"}.join(", ")
 * end
 * def values(n)
 *   (1..n).map{|i| "v#{i-1}"}.join(", ")
 * end
 * def all_types 
 *   types(N)
 * end
 * 
 * def datarow(i) 
 * "case class DataRow#{i}[#{types(i)}](#{values_decl(i)}) extends DataRow((#{values(i)}#{", " if (i < N)}#{(i+1..N).map{"None"}.join(", ")})) {
 *   #{if (i < N) 
 *        "def ![T](v: T) = DataRow#{i+1}[#{types(i)}, T](#{values(i)}, v)"
 *       end}
 * }"  
 * end
 * 
 * def function_def(i)
 *   "(f: Function#{i}[#{types(i)}, R]) = {\n"+
 *   "    val outer = this\n" +
 *   "    function = new Function0[DataTable[#{all_types}]]() { " +
 *   "      def apply(): DataTable[#{all_types}] = {rows foreach {r => executeRow(r, f(#{(1..i).map{|j| "r.values._#{j}"}.join(", "))})}; outer }" +
 *   "    }\n" +
 *   "    if (shouldExecute) execute else this\n" +
 *   "  }"
 * end
 * def lazy_function(i) 
 *   "def |[R]" + function_def(i)
 * end
 * def function(i) 
 *    "def |>[R](f: Function#{i}[#{types(i)}, R]) = {shouldExecute = true; this.|(f)}"
 * end
 * 
 * datarow = "abstract class DataRow[#{all_types}](val values: (#{all_types})) {
 *   var header: TableHeader = TableHeader(Nil)
 *   var shouldExecute: Boolean = false;
 *   def | = this
 *   def |(row: DataRow[#{all_types}]) = DataTable(header, List(this, row), shouldExecute)
 *   def |>(row: DataRow[#{all_types}]) = DataTable(header, List(this, row), true)
 *   override def toString = {
 *     var l: List[Any] = Nil
 *     for (i <- new Range(0, values.productArity, 1);
 *          e <- values.productElement(i) if (e != None))
 *            l = l:::List(e)
 *     l.mkString(\"|\", \"|\", \"|\")
 *   }
 * }"
 * datatable = "case class DataTable[#{all_types}](header: TableHeader, rows: List[DataRow[#{all_types}]], var shouldExecute: Boolean){
 *   var rowResults: String = " " + header.toString
 *   var function : Function0[DataTable[#{all_types}]] = _
 *   def this(rows: List[DataRow[#{all_types}]]) = this(TableHeader(Nil), rows, false)
 *   def |(r: DataRow[#{all_types}]) = DataTable(header, r::rows, shouldExecute) 
 *   def |>(r: DataRow[#{all_types}]) = { this.|(r); shouldExecute = true; this }
 *   def | = this 
 *   def execute = {if (function != null) function.apply(); this}
 *   def results: String = rowResults
 *   private def executeRow(row: List[Any], result: => Any) = {
 *     var failed = false
 *     try { rowResult } 
 *     catch {
 *       case e: Throwable => {
 *         failed = true
 *         rowResults += ("\nx" + row.mkString("|", "|", "|") + " " + e.getMessage)}
 *     }
 *     if (!failed)
 *       rowResults += ("\n" + row.mkString("|", "|", "|"))
 *   }
 *   override def toString = header.toString + \"\\n\" + rows.mkString(\"\\n\")\n"+
 *    (1..N).map{|i| "  " + function(i) + "\n  " + lazy_function(i)}.join("\n") + "}\n"
 *    
 * datarows = (1..N).map{|i| datarow(i)}.join("\n")
 * puts datarow
 * puts datatable
 * puts datarows
 * 
 * 
 */

abstract class DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](val values: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) {
  var header: TableHeader = TableHeader(Nil)
  var shouldExecute: Boolean = false;
  def | = this
  def |(row: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = DataTable(header, List(this, row), shouldExecute)
  def |>(row: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = DataTable(header, List(this, row), true)
  override def toString = {
    var l: List[Any] = Nil
    for (i <- new Range(0, values.productArity, 1);
         e <- values.productElement(i) if (e != None))
           l = l:::List(e)
    l.mkString("|", "|", "|")
  }
}
trait ExecutableDataTable {
  def execute: this.type
  def results: String
}

/**
 * A DataTable contains: a header describing the columns, datarows containing values, a function to execute on each row
 * In the following example of a DataTable:<pre>
 * val datatable = "a" | "b" | "c = a + b" |>
 *                  1  !  2  !     3       | 
 *                  2  !  2  !     4       | 
 *                  3  !  2  !     5       | {
 *                (a: Int, b: Int, c: Int) => c must_== calc.add(a, b)
 *                 } </pre>
 * The header defines 3 columns, there are 3 rows of type (Int, Int, Int) and a function taking its values in each row.
 * The '>' sign added to the header means that the function will be executed on each row when the table is defined. The result
 * of the execution will be available via a <code>results</code> function returning a string.<p>
 * A DataTable has the following constraints:<ul>
 * <li/>It cannot have more than 20 columns
 * <li/>The rows must be of identical types
 * <li/>The function must have the same parameter types as the types of the rows (However the function can have less parameters than the number of row cells)
 * </ul>
 */
case class DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](header: TableHeader, rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]], var shouldExecute: Boolean) extends ExecutableDataTable {
  
  type AbstractDataRow = DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
  
  /**
   * This function can be overriden to provide another behaviour upon table failure
   */  
  def failureFunction(table: DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) : Unit = throw new FailureException(table.results) 
  
  /**
   * @returns the result of the function execution on each row: the string representation of the row and an optional error message in case of a failure
   */  
  var rowResults: List[RowResult] = Nil

  /**
   * function to execute on each row
   */  
  var function : Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = _

  /**
   * Datatable constructor with an empty header
   */  
  def this(rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = this(TableHeader(Nil), rows, false)

  /**
   * Adds a new datarow to the existing table
   */  
  def |(r: AbstractDataRow) = DataTable(header, rows:::List(r), shouldExecute) 

  /**
   * Adds a new datarow to the existing table and sets the table for immediate execution
   */  
  def |>(r: AbstractDataRow) = { this.|(r); shouldExecute = true; this }

  /**
   * closes the table
   */  
  def | = this 

  def failed = rowResults.exists(!_.isOk)
  /**
   * executes the function on each table row  if the function exists
   */  
  def execute = {
    if (function != null) 
      function.apply()
    if (failed) failureFunction(this)
    this
  }

  /**
   * @returns the result of the execution of the table
   */  
  def results: String = {
    val result = 
     header.toString + "\n" +
     rowResults.map(_.toString).mkString("\n")
    
    def alignRows = " " + result.replaceAll("\n\\|", "\n \\|") 
    if (failed) 
      alignRows   
    else 
      result
  }

  /**
   * @returns a string representation of the table
   */  
  override def toString = (header.toString + "\n" + rows.mkString("\n"))
  sealed abstract class RowResult {
    def isOk: boolean
  }
  case class RowOk(row: AbstractDataRow) extends RowResult {
    def isOk = true
    override def toString = row.toString
  }
  
  case class RowKo(row: AbstractDataRow, e: Throwable) extends RowResult {
    def isOk = false
    override def toString = ("x" + row.toString + " " + e.getMessage)
  }
  /**
   * execute a row by checking the execution of the user-supplied function and
   * either displaying the row or displaying the row and an error message
   */  
  private def executeRow(row: AbstractDataRow, rowResult: => Any) = {
    var result: RowResult = RowOk(row)
    try { rowResult } 
    catch {
      case e: Throwable => result = RowKo(row, e)
    }
    rowResults = rowResults:::List(result)
  }
  /**
   * apply a function of one argument to the table and set the table for execution
   */  
  def |>[R](f: Function1[T0, R]) = {shouldExecute = true; this.|(f)}

  /**
   * apply a function of one argument to the table
   */  
  def |[R](f: Function1[T0, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function2[T0, T1, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function2[T0, T1, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function3[T0, T1, T2, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function3[T0, T1, T2, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function4[T0, T1, T2, T3, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function4[T0, T1, T2, T3, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function5[T0, T1, T2, T3, T4, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function5[T0, T1, T2, T3, T4, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19, r.values._20))}; outer }    }
    if (shouldExecute) execute else this
  }
}
case class DataRow1[T0](v0: T0) extends DataRow((v0, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow2[T0, T](v0, v)
}
case class DataRow2[T0, T1](v0: T0, v1: T1) extends DataRow((v0, v1, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow3[T0, T1, T](v0, v1, v)
}
case class DataRow3[T0, T1, T2](v0: T0, v1: T1, v2: T2) extends DataRow((v0, v1, v2, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow4[T0, T1, T2, T](v0, v1, v2, v)
}
case class DataRow4[T0, T1, T2, T3](v0: T0, v1: T1, v2: T2, v3: T3) extends DataRow((v0, v1, v2, v3, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow5[T0, T1, T2, T3, T](v0, v1, v2, v3, v)
}
case class DataRow5[T0, T1, T2, T3, T4](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4) extends DataRow((v0, v1, v2, v3, v4, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow6[T0, T1, T2, T3, T4, T](v0, v1, v2, v3, v4, v)
}
case class DataRow6[T0, T1, T2, T3, T4, T5](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) extends DataRow((v0, v1, v2, v3, v4, v5, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow7[T0, T1, T2, T3, T4, T5, T](v0, v1, v2, v3, v4, v5, v)
}
case class DataRow7[T0, T1, T2, T3, T4, T5, T6](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) extends DataRow((v0, v1, v2, v3, v4, v5, v6, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow8[T0, T1, T2, T3, T4, T5, T6, T](v0, v1, v2, v3, v4, v5, v6, v)
}
case class DataRow8[T0, T1, T2, T3, T4, T5, T6, T7](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T](v0, v1, v2, v3, v4, v5, v6, v7, v)
}
case class DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T8](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v)
}
case class DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v)
}
case class DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v)
}
case class DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v)
}
case class DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v)
}
case class DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v)
}
case class DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, None, None, None, None, None)) {
  def ![T](v: T) = DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v)
}
case class DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, None, None, None, None)) {
  def ![T](v: T) = DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v)
}
case class DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, None, None, None)) {
  def ![T](v: T) = DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v)
}
case class DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, None, None)) {
  def ![T](v: T) = DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v)
}
case class DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, None)) {
  def ![T](v: T) = DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v)
}
case class DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)) {
  
}
