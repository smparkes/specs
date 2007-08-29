package scala.specs;
import scala.io.mock.MockOutput
import scala.specs.integration._

object beforeAfterSuite extends JUnit3(beforeAfterSpec) 
object beforeAfterSpec extends Specification {
  "A specification with before/after clauses" should {
    "have each example using the before method before being executed" in { 
      beforeEx.execute
      beforeEx.messages mustContain "before called 1"
      beforeEx.messages mustContain "before called 2"
    } 
    "not execute its test if the before method fails" in { 
      beforeExampleFailing.execute
      beforeExampleFailing.messages must existMatch("1 error")
      beforeExampleFailing.messages must notExistMatch("tested")
    } 
  }
}

trait beforeAfterTestSpec extends Specification with ConsoleReporter with MockOutput {
  def execute= { suts = Nil; executeSpec; report(suts) }
  def executeSpec
}

object beforeEx extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
  }   
}
object beforeExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => throw new Error("before error") }
      "have example 1 ok" in { Console.println("tested") }
    }
  }   
}
