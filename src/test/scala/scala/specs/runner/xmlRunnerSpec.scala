package scala.specs.runner
import scala.specs.specification._
import scala.util._
import scala.xml._

object xmlSpecRunner extends ConsoleRunner(xmlRunnerSpec)
object xmlRunnerSpec extends RunnerSpecification { 

"The XML runner specification" is <p> 
A specification can be run by a XML runner object. The XML runner object is responsible for
collecting the results of sub-specifications, systems under test and examples and organise 
them hierarchically as xml elements.

<h1>File creation</h1> 

<h2>Simple file creation</h2> 
  
Let's take a simple specification with one system under test and one example: {spec1}
Running an XML runner on that specification{execute(simpleSpecRunner)} should { 
"create a file whose path is " + "./spec1.xml".as(path) in {runner.files must haveKey(path())} 
}

<h2>Output directory</h2> 

It is possible to indicate the output directory of the runner, for example: {"specresults".as(runner.outputDir_=_)}
In that case, {"the xml file should be created in the output directory with path: " + 
               ("./" + runner.outputDir + "/spec1.xml").as(path) in {
  runner.reset
  runner.execute
  runner.files must haveKey(path.toString)}
}

<h1>XML content</h1> 

Running an XML runner on a specification should create an xml structure containing an element for:  

{"the specification:\n" +
<spec name="spec1" description="spec1" assertions="3" failures="1" errors="1"></spec>.as(xml) in checkXml }

{"the system under test:\n" +
<sut description="the sut" assertions="3" failures="1" errors="1"></sut>.as(xml) in checkXml }

{"the ok example test:\n" +
<example description="have one ok example" assertions="0" failures="0" errors="0"></example>.as(xml) in checkXml }

{"the ko example test containing the failure:\n" +
    <example description="have one ko example" assertions="0" failures="1" errors="0">
      <failure>'1' is not equal to '2'</failure>
    </example>.as(xml) in checkXml }

{"the ko example test containing the exception:\n" +
    <example description="have an example with an exception" assertions="0" failures="0" errors="1">
      <exception>error message</exception>
    </example>.as(xml) in checkXml }

{"the example containing a sub-example:\n" +
    <example description="have one sub-example" assertions="1" failures="0" errors="0">
      <example description="a sub-example" assertions="1" failures="0" errors="0"></example>
    </example>.as(xml) in checkXml }

If the XML runner is run on a composite specification{execute(compositeSpecRunner)} it must then {"create a specification element containing the sub-specifications:\n" +
<spec name="compositeSpec" description="compositeSpec" assertions="6" failures="2" errors="2"></spec>.as(xml) in checkXml
<spec name="spec1" description="spec1" assertions="3" failures="1" errors="1"></spec>.as(xml) in checkXml                                                                                                  
}
</p>  


}

trait RunnerSpecification extends BizSpecification {
  import scala.io.mock._
  import scala.io._
  import scala.specs.specification._
  var path = Property("")
  var xml: Property[Elem] = Property(<p></p>).toString(e => new PrettyPrinter(200, 2).format(e))
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
  def execute(r: XmlRunner with MockFileSystem) = {runner = r; r.reset; r.execute.shh}
  def checkXml = { XML.loadString(runner.readFile(runner.files.keys.next)) must \\(xml()) }
}
