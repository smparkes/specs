package scala.specs.runner

import scala.specs._
import scala.util._
import scala.collection.mutable.Queue
import scala.io.mock.MockOutput
import scala.specs.integration._

object specsRunnerSuite extends JUnit3(specsRunnerSpec)
object specsRunnerSpec extends Specification with TestRunner {
  "A specs runner" should {
    usingBefore { () => runner.messages.clear }
    
    "execute a specification contained in a file" in { 
      runWith("scala.specs.sampleSpec1")
      messages mustExistMatch "example"
    }
    "execute 2 specifications contained in a directory" in { 
      runWith("scala.specs.sampleSpec1", "scala.specs.sampleSpec2")
      messages mustExistMatch "specification1"
      messages mustExistMatch "specification2"
    }
  }
}

trait MockSpecsFinder extends SpecsFinder {
  var paths: List[String] = _ 
  override def specificationNames(filesPath: String) = paths
}
trait TestRunner {
  object runner extends SpecsFileRunner with ConsoleReporter with MockSpecsFinder with MockOutput
  def runWith(paths: String*) = {
    runner.paths = paths.toList 
    runner.runSpecs("real path doesn't mind here")
  }
  def messages = runner.messages
}
 


