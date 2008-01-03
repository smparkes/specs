package org.specs.runner

import org.specs._
import org.specs.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._

class specsRunnerTest extends JUnit3(specsRunnerSpec)
object specsRunnerSpec extends Specification with TestRunner {
  "A specs runner" should {
    usingBefore { () => runner.messages.clear }
    
    "execute a specification contained in a file" in { 
      runWith("org.specs.samples.sampleSpec1$")
      messages mustHaveMatch "example"
    }
    "execute 2 specifications contained in a directory" in { 
      runWith("org.specs.samples.sampleSpec1$", "org.specs.samples.sampleSpec2$")
      messages foreach {m =>  println(m)}
      messages mustHaveMatch "specification1"
      messages mustHaveMatch "specification2"
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
object AllSpecsFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Spec") 
object AllUnitFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Unit") 
object AllFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*.*") 


