package org.specs.runner
import scala.xml._
import org.specs.specification._
import org.specs.io._
import org.specs.util._

class HtmlRunner(specification: Specification, outputDir: String) extends Xml {
  outputDirPath = normalize(outputDir)
  override def fileName = "specs-report.html"

  val specs: Seq[Specification] = List(specification)
  
  def this(spec: Specification) = this(spec, ".")

  override def report(specs: Iterable[Specification]) = {}
  override def reportSpec = {
    super.reportSpec
    copySpecResourcesDir("images", outputDirPath)
    copySpecResourcesDir("css", outputDirPath)
  }
 
  override def specOutput = asHtml(specs(0))
  
  def asHtml(spec: Specification): Elem = <html>
    <head>
      <title>{spec.name}</title>
	    <style type="text/css" media="all">
	      @import url('./css/maven-base.css');
	      @import url('./css/maven-theme.css');
	      @import url('./css/site.css');
	    </style>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>
    <body>
    <div id="bodyColumn">
    {subspecsTables(spec.subSpecifications)}
    {sutTables(spec.suts)}
    </div>
    </body>
  </html>
  
  def subspecsTables(subSpecs: List[Specification]): NodeSeq = subSpecs.foldRight(NodeSeq.Empty.toSeq) { (subSpec, node) => 
    node ++ subSpecTable(subSpec) 
  }
  def subSpecTable(subSpec: Specification) = {
    <h2>{subSpec.description}</h2> ++ subspecsTables(subSpec.subSpecifications) ++ sutTables(subSpec.suts)
  }
  def sutTables(suts: List[Sut]): NodeSeq = suts.foldRight(NodeSeq.Empty.toSeq) { (sut, node) => node ++  sutTable(sut) }
  
  def sutTable(sut: Sut): NodeSeq = <h3>{sut.header}</h3>.toSeq ++ <table class="bodyTable">
    {exampleRows(sut.examples)}
    </table>
    
  def exampleRows(examples: Iterable[Example]) = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) => 
    val (node, alternation) = result
    (node ++ exampleRow(ex, alternation), !alternation) 
  }._1
  
  def exampleRow(example: Example, alternation: Boolean) = <tr class={if (alternation) "b" else "a"}>
    <td>{statusIcon(example)}{example.description}</td><td>{message(example)}</td></tr>
    
  def statusIcon(example: Example) = {
    if (!example.failures.isEmpty)
      <img src="images/icon_warning_sml.gif"/>
    else if (!example.errors.isEmpty)
      <img src="images/icon_error_sml.gif"/>
    else if (!example.skipped.isEmpty)
      <img src="images/icon_info_sml.gif"/>
    else
      <img src="images/icon_success_sml.gif"/>
  }
  
  def message(example: Example) = {
    if (!example.failures.isEmpty)
      example.failures.foldLeft(NodeSeq.Empty.toSeq)( (res, f) => res ++ failure(f))
    else if (!example.errors.isEmpty)
      example.errors.foldLeft(NodeSeq.Empty.toSeq)( (res, e) => res ++ new Text(e.getMessage))
    else if (!example.skipped.isEmpty)
      example.skipped.foldLeft(NodeSeq.Empty.toSeq)( (res, s) => res ++ new Text(s.getMessage))
    else
      ""
  }
  def failure(f: FailureException): NodeSeq = {
    f match {
      case DataTableFailureException(table) => xmlFor(table)
      case regular => new Text(regular.getMessage) 
    }
  }
  type DT = DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] forSome { type T0; type T1; type  T2; type T3; type T4; type T5; type T6; type T7; type T8; type T9; type T10; type T11; type T12; type T13; type T14; type T15; type T16; type T17; type T18; type T19 } 
  def xmlFor(table: DT) = {
    val header = table.header.titles.foldLeft(<td/>.toSeq)( (res, s) => res ++ <td>{s}</td>)
    def tableResult(rowResult: RowResult) = {
      status(rowResult) ++
      rowResult.row.valuesList.foldLeft(NodeSeq.Empty.toSeq)( (res, value) => res ++ <td>{value.toString}</td>) ++
      failureMessage(rowResult)
    }
    def failureMessage(rowResult: RowResult) = {
      rowResult match {
        case RowOk(_) => NodeSeq.Empty
        case RowKo(row, failure) => <tr><td/><td colspan={row.valuesList.size.toString} class="failureMessage">{failure.getMessage}</td></tr>
      }
    }
    def status(rowResult: RowResult) = {
      if (rowResult.isOk) <td/>
      else <td class="noBorder"><img src="images/icon_warning_sml.gif"/></td>
    }
    val tableResults = {
      table.rowResults.foldLeft(NodeSeq.Empty.toSeq)( (res, r) => res ++ <tr>{tableResult(r)}</tr>)
    }
    <table class="nested">{header ++ tableResults }</table>
  }
}