package org.specs.specification
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs._
import org.specs.runner._

class beforeAfterTest extends Runner(beforeAfterSpec) with JUnit 
object beforeAfterSpec extends Specification {
  "A specification with before clauses" should {
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
    "throw an exception if the doBefore clause is not declared inside a sut" in { 
      object badSpec extends Specification {
        doBefore {}
      }
      badSpec must throwA(new FailureException(""))
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
    "deprecated - throw an exception if the usingBefore clause is not declared inside a sut" in { 
      object badSpec extends Specification {
        usingBefore {() => ()}
      }
      badSpec must throwA(new FailureException(""))
    } 
  }
  "A specification with after clauses" should {
    "have each example using the doAfter method after being executed" in { 
      doAfterExample.execute
      doAfterExample.messages mustContain "after called 1"
      doAfterExample.messages mustContain "after called 2"
    } 
    "not execute its test if the doAfter method fails" in { 
      doAfterExampleFailing.execute
      doAfterExampleFailing.messages must existMatch("1 error")
      doAfterExampleFailing.messages must notExistMatch("tested")
    }
    "throw an exception if the doAfter clause is not declared inside a sut" in { 
      object badSpec extends Specification {
        doAfter {}
      }
      badSpec must throwA(new FailureException(""))
    } 
    "deprecated - have each example using the usingAfter method after being executed" in { 
      afterEx.execute
      afterEx.messages mustContain "after called 1"
      afterEx.messages mustContain "after called 2"
    } 
    "deprecated - not execute its test if the usingAfter method fails" in { 
      afterExampleFailing.execute
      afterExampleFailing.messages must existMatch("1 error")
      afterExampleFailing.messages must notExistMatch("tested")
    } 
    "deprecated - throw an exception if the usingAfter clause is not declared inside a sut" in { 
      object badSpec extends Specification {
        usingAfter {() => ()}
      }
      badSpec must throwA(new FailureException(""))
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
object doAfterExample extends beforeAfterTestSpec {
  override def executeSpec = {
    var afterCalls = 0
    "A specification" should { doAfter { afterCalls += 1; println("after called " + afterCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpec(this)
  }   
}
object doAfterExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
      var afterCalls = 0
    "A specification" should { doAfter { error("after error") }
      "have example 1 ok" in { Console.println("tested") }
    }
    reportSpec(this)
  }   
}
object afterEx extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var afterCalls = 0
      usingAfter { () => afterCalls += 1; println("after called " + afterCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpec(this)
  }   
}
object afterExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var afterCalls = 0
      usingAfter { () => error("after error") }
      "have example 1 ok" in { Console.println("tested") }
    }
    reportSpec(this)
  }   
}
