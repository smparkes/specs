package org.specs.runner
import org.specs.specification._
import org.specs.util._
import scala.xml._

object xmlRunnerSpec extends RunnerFixture { "The specification for the XML runner" is <html> 

  A specification can be run by a XML runner object. The XML runner object is responsible for
  collecting the results of sub-specifications, systems under test and examples and organize 
  them hierarchically as xml elements.

<p/>  1. File creation 
<p/>  1.1 Simple file creation 
  
<p/> Running an XML runner on a specification should create a file whose path is the name of the specification.
  For example, running the specification named "sp1" should create the path {
    "./org.specs.runner.sp1.xml".as(path) + " should be the name of the specification" in checkFilePath }

<p/>  1.2 Output directory

<p/>  It is possible to indicate the output directory of the runner, for example: {"specresults" as runnerOutputDir}
      In that case, {"the xml file should be created in the output directory with path: " + 
                 "./specresults/org.specs.runner.sp1.xml".as(path) in checkOutputDirectory}

<p/> 2. XML content 

<p/>  Running an XML runner on a specification should create an xml structure:  
<ul>
<li> {"containing an element for the specification:\n" +
  <spec name="sp1" description="sp1" assertions="3" failures="1" errors="1"></spec>.as(xml) in checkXml }</li>

<li> {"containing an element for the system under test:\n" +
  <sus description="the sus" assertions="3" failures="1" errors="1"></sus>.as(xml) in checkXml }</li>

<li> {"containing an element for the ok example test:\n" +
  <example description="have one ok example" assertions="0" failures="0" errors="0"></example>.as(xml) in checkXml }</li>

<li> {"containing an element for the ko example test containing the failure:\n" +
  <example assertions="1" failures="1" description="have one ko example" errors="0">
          <failure location="xmlRunnerFixture.scala:49">'1' is not the same as '2'</failure>
    </example>.as(xml) in checkXml }</li>

<li> {"containing an element for the ko example test containing the exception:\n" +
  <example description="have an example with an error" assertions="1" failures="0" errors="1">
      <error location="xmlRunnerFixture.scala:50">error message</error>
    </example>.as(xml) in checkXml }</li>

<li> {"containing an element for the example containing a sub-example:\n" +
  <example assertions="0" failures="0" description="have one sub-example" errors="0">
          <example assertions="1" failures="0" description="a sub-example" errors="0"></example>
    </example>.as(xml) in checkXml }</li>
</ul>
<p/>
  If the XML runner is run on a composite specification{executeCompositeSpecRunner},
  it must then create a specification element containing the sub-specifications <ul>
 
<li><ex>with one node for the first sub-specification</ex>
     { <spec name="compositeSpec" description="compositeSpec" assertions="6" failures="2" errors="2"/> as xml}{checkXml}
<li><ex>with another node for the other sub-specification</ex></li>
     { <spec name="sp1" description="sp1" assertions="3" failures="1" errors="1"/> as xml}{checkXml} </li>                                                                                                  
</ul>

<p/>  3. Console reporting

<p/> The XML runner object should {"output the results of the specification in the console" in checkConsole}, 
     as if it was a ConsoleRunner if it has been added the Console trait.

</html>  
}
