package scala.specs.runner
import scala.io._
import scala.util._
import scala.log._
import scala.specs._
import java.io.Writer
import scala.xml.{Elem, PrettyPrinter}
import scala.specs.specification._

/**
 * The <code>XmlRunner</code> class is used to create an xml file, in a specified output directory
 * with the results of a specification execution.
 * Usage: <code>object runner extends XmlRunner(mySpec, "./results/specs")</code>
 * The name of the generated file is specification.name by default but can be overriden:<pre>
 * object runner extends XmlRunner(mySpec, "./results/specs"){override val fileName="result"}</pre>
 * If the output directory is not specified <pre>object runner extends XmlRunner(mySpec)</pre> then the
 * current directory will be used
 */
class XmlRunner(specification: Specification, var outputDir: String) extends Reporter with FileSystem with ConsoleLog with FileWriter {
  val fileName = specification.name
  var filePath = normalize(outputDir) + fileName + ".xml"
  def main(args: Array[String]): Unit = execute
  def this(specification: Specification) = this(specification, ".")
  def normalize(dir: String) = {
    var properDir = dir.replaceAll("\\\\", "/")
    if (!properDir.startsWith("/") && !properDir.startsWith("."))
      properDir = ("./" + properDir)
    if (!properDir.endsWith("/"))
      properDir += "/"
    properDir
  }
  def execute = {
    createFile(filePath)
    write(filePath) { out: Writer =>
      out.write(new PrettyPrinter(200, 2).format(asXml(specification)))
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
  def report(specifications: Iterable[Specification]) = {}
}
