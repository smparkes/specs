package scala.specs.runner
import scala.io._
import scala.util._
import scala.log._
import scala.specs._
import java.io.Writer
import scala.xml.{Elem, PrettyPrinter}
import scala.specs.specification._

class XmlRunner(s: Specification, var outputDir: String) extends Reporter with FileSystem with ConsoleLog with FileWriter {
  var fileName = "" 
  def main(args: Array[String]): Unit = execute
  def this(s: Specification) = this(s, ".")
  def execute = {
    outputDir = outputDir.replaceAll("\\\\", "/")
    if (!outputDir.startsWith("/") && !outputDir.startsWith("."))
      outputDir = ("./" + outputDir)
    if (!outputDir.endsWith("/"))
      outputDir += "/"
    fileName = outputDir + s.name + ".xml"
    createFile(fileName)
    write(fileName) { out: Writer =>
      out.write(new PrettyPrinter(200, 2).format(asXml(s)))
    }
  }
  def asXml(s: Specification): Elem =
    <spec name={s.name} description={s.description} assertions={s.assertionsNb.toString} failures={s.failures.size.toString} errors={s.errors.size.toString}>
      {s.subSpecifications map (asXml(_))}{
       s.suts map (asXml(_))}
    </spec>
  def asXml(sut: Sut): Elem = 
    <sut description={sut.description} assertions={sut.assertionsNb.toString} failures={sut.failures.size.toString} errors={sut.errors.size.toString}>
      {sut.examples map (asXml(_))}
    </sut>
  def asXml(e: Example): Elem = 
    <example description={e.description} assertions={e.assertionsNb.toString} failures={e.failures.size.toString} errors={e.errors.size.toString}>{
      e.failures map (asXml(_))}{
      e.subExamples map (asXml(_))
    }</example>

  def asXml(failure: FailureException): Elem = 
    <failure>{failure.message}</failure>
  def report(s: Iterable[Specification]) = {}
}
