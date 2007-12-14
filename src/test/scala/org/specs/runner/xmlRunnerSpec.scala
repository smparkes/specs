package org.specs.runner
import org.specs.specification._
import org.specs.util._
import scala.xml._

object xmlRunnerSuite extends JUnit3(xmlRunnerSpec)
object xmlSpecRunner extends ConsoleRunner(xmlRunnerSpec)
object xmlRunnerSpec extends RunnerFixture { 

"The specification for the XML runner" is <p> 
A specification can be run by a XML runner object. The XML runner object is responsible for
collecting the results of sub-specifications, systems under test and examples and organize 
them hierarchically as xml elements.

1. File creation 
1.1 Simple file creation 
  
{"Running an XML runner on a specification should create a file whose path is " + "./spec1.xml".as(path) in {
  checkFilePath}}

1.2 Output directory
It is possible to indicate the output directory of the runner, for example: {"specresults" as runnerOutputDir}
In that case, {"the xml file should be created in the output directory with path: " + 
               ("./" + runner.outputDir + "/spec1.xml").as(path) in checkOutputDirectory}

2. XML content 

Running an XML runner on a specification should create an xml structure:  

- {"containing an element for the specification:\n" +
  <spec name="spec1" description="spec1" assertions="3" failures="1" errors="1"></spec>.as(xml) in checkXml }

- {"containing an element for the system under test:\n" +
  <sut description="the sut" assertions="3" failures="1" errors="1"></sut>.as(xml) in checkXml }

- {"containing an element for the ok example test:\n" +
  <example description="have one ok example" assertions="0" failures="0" errors="0"></example>.as(xml) in checkXml }

- {"containing an element for the ko example test containing the failure:\n" +
  <example assertions="1" failures="1" description="have one ko example" errors="0">
          <failure location="xmlRunnerSpec.scala:93">'1' is not the same as '2'</failure>
    </example>.as(xml) in checkXml }

- {"containing an element for the ko example test containing the exception:\n" +
  <example description="have an example with an error" assertions="1" failures="0" errors="1">
      <error location="xmlRunnerSpec.scala:94">error message</error>
    </example>.as(xml) in checkXml }

- {"containing an element for the example containing a sub-example:\n" +
  <example assertions="0" failures="0" description="have one sub-example" errors="0">
          <example assertions="1" failures="0" description="a sub-example" errors="0"></example>
    </example>.as(xml) in checkXml }

If the XML runner is run on a composite specification{executeCompositeSpecRunner},
it must then {"create a specification element containing the sub-specifications:\n" +                
<spec name="compositeSpec" description="compositeSpec" assertions="6" failures="2" errors="2"></spec>.as(xml) in checkXml
<spec name="spec1" description="spec1" assertions="3" failures="1" errors="1"></spec>.as(xml) in checkXml                                                                                                  
}

</p>  
}

trait RunnerFixture extends LiteralSpecification with RunnerTestData {
  def createSimpleSpecRunner = runner = simpleSpecRunner
  def executeCompositeSpecRunner = {runner = compositeSpecRunner; executeRunner}
  def executeRunner = {runner.reset; runner.execute.shh}
  def runnerOutputDir = runner.outputDir_=_
  def checkXml = XML.loadString(runner.readFile(runner.files.keys.next)) must \\(xml()) 
 
  def checkFilePath = {
    createSimpleSpecRunner
    executeRunner
    runner.files must haveKey(path())
  }
  def checkOutputDirectory = {
    runner.reset
    runner.execute
    runner.files must haveKey(path.toString)
  }
}
trait RunnerTestData {
  import org.specs.io.mock._
  import org.specs.io._
  import org.specs.specification._
  var path = Property("")
  var xml: Property[Elem] = Property(<p></p>).onToString(e => new PrettyPrinter(200, 2).format(e))
  var runner: XmlRunner with MockFileSystem = _
  object simpleSpecRunner extends XmlRunner(spec1) with MockFileSystem
  object spec1 extends Specification {
    override def toString = name
    "the sut" should {
      "have one ok example" in {1 mustBe 1}
      "have one ko example" in {1 mustBe 2}
      "have an example with an exception" in {throw new Error("error message")}
      "have one sub-example" in { "a sub-example" in {1 mustBe 1}}
    }
  }
  object compositeSpecRunner extends XmlRunner(compositeSpec) with MockFileSystem
  object compositeSpec extends Specification { 
    "a composite spec" isSpecifiedBy(spec1, spec1)
  }
}