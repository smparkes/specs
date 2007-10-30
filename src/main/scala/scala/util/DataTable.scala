package scala.util
import scala.specs.Sugar._

trait DataTables {
  implicit def toTableHeader(a: String) = TableHeader(List(a))
  implicit def toDataRow[T](a: T) = DataRow1(a)
}
case class TableHeader(h: List[String]) {
  def | = this
  def |[T <: Any {def header_=(t: TableHeader)}](d: T) = {d.header_=(this); d}
  def |>[T <: Any {def header_=(t: TableHeader); def shouldExecute_=(b: Boolean)}](d: T) = {d.header_=(this); d.shouldExecute_=(true); d}
  def !(s: String) = TableHeader(h:::List(s))
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
 *   "      def apply(): DataTable[#{all_types}] = {rows foreach {r => f(#{(1..i).map{|j| "r.values._#{j}"}.join(", ")})}; outer }" +
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
 *   var function : Function0[DataTable[#{all_types}]] = _
 *   def this(rows: List[DataRow[#{all_types}]]) = this(TableHeader(Nil), rows, false)
 *   def |(r: DataRow[#{all_types}]) = DataTable(header, r::rows, shouldExecute) 
 *   def |>(r: DataRow[#{all_types}]) = { this.|(r); shouldExecute = true; this }
 *   def | = this 
 *   def execute = {if (function != null) function.apply(); this}
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
case class DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](header: TableHeader, rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]], var shouldExecute: Boolean){
  var function : Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = _
  def this(rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = this(TableHeader(Nil), rows, false)
  def |(r: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = DataTable(header, r::rows, shouldExecute) 
  def |>(r: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = { this.|(r); shouldExecute = true; this }
  def | = this 
  def execute = {if (function != null) function.apply(); this}
  override def toString = header.toString + "\n" + rows.mkString("\n")
  def |>[R](f: Function1[T0, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function1[T0, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function2[T0, T1, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function2[T0, T1, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function3[T0, T1, T2, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function3[T0, T1, T2, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function4[T0, T1, T2, T3, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function4[T0, T1, T2, T3, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function5[T0, T1, T2, T3, T4, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function5[T0, T1, T2, T3, T4, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19)}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {
    val outer = this
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19, r.values._20)}; outer }    }
    if (shouldExecute) execute else this
  }}
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
