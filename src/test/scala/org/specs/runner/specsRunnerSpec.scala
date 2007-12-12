package scala.specs.runner

import org.specs._
import scala.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._

object specsRunnerSuite extends JUnit3(specsRunnerSpec)
object specsRunnerSpec extends Specification with TestRunner {
  "A specs runner" should {
    usingBefore { () => runner.messages.clear }
    
    "execute a specification contained in a file" in { 
      runWith("scala.specs.samples.sampleSpec1$")
      messages mustExistMatch "example"
    }
    "execute 2 specifications contained in a directory" in { 
      runWith("scala.specs.samples.sampleSpec1$", "scala.specs.samples.sampleSpec2$")
      messages mustExistMatch "specification1"
      messages mustExistMatch "specification2"
    }
  }
}

trait MockSpecsFinder extends SpecsFinder {
  var classNames: List[String] = Nil
  override def specificationNames(filesPath: String, pattern: String) = classNames
}
trait TestRunner {
  object runner extends SpecsFileRunner("", ".*") with ConsoleReporter with MockSpecsFinder with MockOutput
  def runWith(classNames: String*) = {
    runner.classNames = classNames.toList 
    runner.report(Nil)
  }
  def messages = runner.messages
}
object AllSpecsFileRunner extends SpecsFileRunner("./src/test/scala/scala/specs", "([^(?>all)].)*Spec") 
object AllUnitFileRunner extends SpecsFileRunner("./src/test/scala/scala/specs", "([^(?>all)].)*Unit") 
object AllFileRunner extends SpecsFileRunner("./src/test/scala/scala/specs", "([^(?>all)].)*.*") 


