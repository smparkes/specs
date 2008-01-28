package org.specs.specification
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs._
import org.specs.runner._

class beforeAfterTest extends JUnit3(beforeAfterSpec) 
object beforeAfterSpecRunner extends ConsoleRunner(beforeAfterSpec)
object beforeAfterSpec extends Specification {
  "A specification with before/after clauses" should {
    "have each example using the doBefore method before being executed" in { 
      doBeforeExample.execute
      doBeforeExample.messages mustContain "before called 1"
      doBeforeExample.messages mustContain "before called 2"
    } 
    "not execute its test if the doBefore method fails" in { 
      doBeforeExampleFailing.execute
      doBeforeExampleFailing.messages must existMatch("1 error")
      doBeforeExampleFailing.messages must notExistMatch("tested")
    }
    "deprecated - have each example using the usingBefore method before being executed" in { 
      beforeEx.execute
      beforeEx.messages mustContain "before called 1"
      beforeEx.messages mustContain "before called 2"
    } 
    "deprecated - not execute its test if the usingBefore method fails" in { 
      beforeExampleFailing.execute
      beforeExampleFailing.messages must existMatch("1 error")
      beforeExampleFailing.messages must notExistMatch("tested")
    } 
  }
}

trait beforeAfterTestSpec extends Specification with ConsoleReporter with MockOutput {
  def execute = { suts = Nil; executeSpec }
  def executeSpec
}

object doBeforeExample extends beforeAfterTestSpec {
  override def executeSpec = {
    var beforeCalls = 0
    "A specification" should { doBefore { beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpec(this)
  }   
}
object doBeforeExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
      var beforeCalls = 0
    "A specification" should { doBefore { error("before error") }
      "have example 1 ok" in { Console.println("tested") }
    }
    reportSpec(this)
  }   
}
object beforeEx extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpec(this)
  }   
}
object beforeExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => error("before error") }
      "have example 1 ok" in { Console.println("tested") }
    }
    reportSpec(this)
  }   
}
