package scala.specs.runner
import org.specs.io._
import scala.util._
import scala.log._
import org.specs._
import java.io.Writer
import scala.xml.{Elem, PrettyPrinter}
import org.specs.specification._

/**
 * The <code>XmlRunner</code> class is used to create an xml file, in a specified output directory
 * with the results of a specification execution.
 * Usage: <code>object runner extends XmlRunner(mySpec, "./results/specs")</code>
 * The name of the generated file is specification.name by default but can be overriden:<pre>
 * object runner extends XmlRunner(mySpec, "./results/specs"){override val fileName="result"}</pre>
 * If the output directory is not specified <pre>object runner extends XmlRunner(mySpec)</pre> then the
 * current directory will be used
 */
class XmlRunner(specification: Specification, var outputDir: String) extends FileSystem with ConsoleLog with FileWriter {
  /**
   * Alternate constructor with the specification only. The output dir is the current directory
   */
  def this(specification: Specification) = this(specification, ".")

  /**
   * the default name of the file is the specification name 
   */
  val fileName = specification.name
  
  /**
   * the default path is the output dir + specification name + .xml 
   */
  def filePath = normalize(outputDir) + fileName + ".xml"

  /**
   * calling main should execute the runner 
   */
  def main(args: Array[String]): Unit = execute
  
  /**
   * creates the file and write the xml result of the specification execution 
   */
  def execute = {
    createFile(filePath)
    write(filePath) { out: Writer =>
      out.write(new PrettyPrinter(200, 2).format(asXml(specification)))
    }
  }

  /**
   * @returns a path with Unix like path separators and a final / separator 
   */
  def normalize(dir: String) = {
    var properDir = dir.replaceAll("\\\\", "/")
    if (!properDir.startsWith("/") && !properDir.startsWith("."))
      properDir = ("./" + properDir)
    if (!properDir.endsWith("/"))
      properDir += "/"
    properDir
  }

  /**
   * @returns the specification results translated as to xml (including subspecifications)
   */
  def asXml(s: Specification): Elem =
    <spec name={s.name} description={s.description} assertions={s.assertionsNb.toString} failures={s.failures.size.toString} errors={s.errors.size.toString}>
      {s.subSpecifications map (asXml(_))}{
       s.suts map (asXml(_))}
    </spec>

  /**
   * @returns the sut results translated as to xml 
   */
  def asXml(sut: Sut): Elem = 
    <sut description={sut.description} assertions={sut.assertionsNb.toString} failures={sut.failures.size.toString} errors={sut.errors.size.toString}>
      {sut.examples map (asXml(_))}
    </sut>

  /**
   * @returns the example results translated as to xml (including sub-examples) 
   */
  def asXml(e: Example): Elem = 
    <example description={e.description} assertions={e.assertionsNb.toString} failures={e.failures.size.toString} errors={e.errors.size.toString}>{
      e.failures map (asXml(_))}{
      e.errors map (asXml(_))}{
      e.subExamples map (asXml(_))
    }</example>

  /**
   * @returns an error translated as to xml 
   */
  def asXml(error: Throwable): Elem = <error>{error.getMessage}</error>

  /**
   * @returns a failure translated as to xml 
   */
  def asXml(failure: FailureException): Elem = <failure>{failure.message}</failure>

}
