package org.specs.mock
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._
import org.hamcrest.core._
import org.specs.matcher._

class jmockSpecTest extends JUnit3(jmockSpec)
object jmockSpecRunner extends ConsoleRunner(jmockSpec)

object jmockSpec extends Specification {
  "The jMock integration".isSpecifiedBy(jmockGoodSpec, jmockBadSpec)
}
object jmockGoodSpec extends Mocked {
  "The JMocker trait" should {
    "provide a 'one' method succeeding if only one method is called" in {
      expect { one(list).size }
      list.size()
    } 
    "provide an 'exactly' method succeeding if exactly the right number of calls are made" in {
      expect { 2.of(list).size }
      2 times {i => list.size}
    } 
    "provide an 'atLeast' method succeeding if atLeast the right number of calls are made" in {
      expect { 2.atLeastOf(list).size }
      3 times {i => list.size}
    } 
    "provide a 'between' method succeeding if the number of calls is between inside a range" in {
      expect { (2 to 4).of(list).size }
      3 times {i => list.size}
    } 
    "provide an 'atMost' method succeeding if at most the right number of calls are made" in {
      expect { 2.atMostOf(list).size }
      2 times {i => list.size}
    } 
    "provide an 'allowing' method succeeding if any number of calls is made" in {
      expect { allowing(list).size }
      2 times {i => list.size}
    } 
    "provide an 'allowingMatch' method succeeding if calls match a method pattern" in {
      expect { allowingMatch("size") }
      list.size
    } 
    "provide an 'allowingMatch' method succeeding if calls match a method pattern on a specific mock" in {
      expect { allowingMatch(list, "size") }
      list.size
    } 
    "provide an 'ignoring' method accepting any call and returning default values" in {
      expect { ignoring(list) }
      list.size must_== 0
    } 
    "provide an 'ignoring' method where toString on a mock returns the mock name build following the class name" in {
      expect { ignoring(list) }
      list.toString must_== "list"
    } 
    "provide an 'ignoringMatch' method accepting any call matching a method pattern and returning default values" in {
      expect { ignoringMatch("size") }
      list.size must_== 0
    } 
    "provide a 'never' method succeeding if no call is made to the mock" in {
      expect { never(list) }
    } 
    "provide an anyInt matcher which can be used to specify that any Int will be used as a parameter" in {
      expect { 1.of(list).get(anyInt) }
      list.get(0)
    } 
    "provide an equal matcher which can be used to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(equal(0)) }
      list.get(0)
    } 
    "provide a will method, using a Hamcrest matcher, to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(will(new IsEqual(0))) }
      list.get(0)
    } 
    "provide a will method, using a specs matcher, to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(will(beEqual(0))) }
      list.get(0)
    } 
    "provide a willReturn method to specify the value which must be returned" in {
      expect { 1.of(list).get(will(beEqual(0))) willReturn "new" }
      list.get(0) must_== "new"
    } 
    "provide a willThrow method to specify the exception which must be thrown" in {
      expect { 1.of(list).get(will(beEqual(0))) willThrow new java.lang.Exception("ouch") }
      list.get(0) must throwA(new Exception)
    } 
    "provide a willReturn method to specify the a returned iterator" in {
      val expected = List[String]("hey")
      expect { 1.of(scalaList).elements willReturn expected.elements }
      scalaList.elements.next must_== "hey"
    }
    "provide a willReturn method to specify a returned iterable" in {
      expect { 1.of(scalaList).take(anyInt) willReturn List("hey") }
      scalaList.take(1) must existMatch("hey")
    }
    "provide a willReturn method accepting a block to return another mock and specify it too" in {
      case class Module(name: String)
      case class Project(module: Module, name: String)
      case class Workspace(project: Project)
      val workspace = mock(classOf[Workspace])
 
      expect { 
        1.atLeastOf(workspace).project.willReturn(classOf[Project]) {p: Project => 
           1.atLeastOf(p).name willReturn "hi"
           1.atLeastOf(p).module.willReturn(classOf[Module]){m: Module => 1.of(m).name willReturn "module"}
        }
      }
      workspace.project.name must_== "hi"
      workspace.project.module.name must_== "module"
    } 
    "provide a willReturn method returning iterables and accepting a block to return another mock and specify it too" in {
      case class Project(name: String)
      case class Workspace(projects: List[Project])
      val workspace = mock(classOf[Workspace]) 
      expect { 
        one(workspace).projects willReturnIterable(
          as(classOf[Project], "p1"){p: Project => 
            one(p).name willReturn "p1" },
          as(classOf[Project], "p2"){p: Project => 
            one(p).name willReturn "p2" }
        )
      }
      workspace.projects.map(_.name) must_== List("p1", "p2")
    } 
    "provide a willReturn method returning iterables and accepting a block to return another mock and specify it too - 2" in {
      case class Project(name: String)
      case class Workspace(projects: List[Project])
      val workspace = mock(classOf[Workspace]) 
      expect { 
        one(workspace).projects willReturnIterable(classOf[Project], 
           {p: Project => one(p).name willReturn "p1" },
           {p: Project => one(p).name willReturn "p2" })
      }
      workspace.projects.map(_.name) must_== List("p1", "p2")
    } 
    "provide a will method to add any action to an expectation" in {
      expect { 
        one(list).get(anyInt) will(returnValue("hey")) 
      }
      list.get(0) must_== "hey"
    } 
    "provide a will method to return different values on consecutive calls" in {
      expect { 
        1.atLeastOf(list).get(anyInt) willReturnEach ("a", "b", "c")
      }
      ("a", "b", "c") foreach { list.get(0) must_== _}
    } 
    "provide a then method to constraint calls to occur in sequence" in {
      expect { 
        one(list).size then 
        one(list).get(anyInt) then
        one(list).isEmpty
      }
      list.size
      list.get(0)
      list.isEmpty
    } 
    "provide a when/set methods to constraint calls to occur in specific states" in {
      val readiness = state("readiness")
      readiness.startsAs("not ready")
      expect { 
        one(list).size set readiness.is("ready") 
        allowing(list).get(anyInt) when readiness.is("ready")
      }
      list.size
      list.get(0)
    } 
  }
}
object jmockBadSpec extends BadMocked {
  "The JMocker trait" should {
    "provide a 'one' method failing if no method is called" in {
       expect { one(list).size }
    } 
    "provide an 'exactly' method failing if a lesser number of calls are made" in {
       expect { exactly(2).of(list).size }
       list.size
    } 
    "provide an 'exactly' method failing if a bigger number of calls are made" in {
       expect { exactly(2).of(list).size }
       3 times {i => list.size}      
    }  
    "provide an 'atLeast' method failing if less than the right number of calls are made" in {
       expect { atLeast(2).of(list).size }
       list.size
    } 
    "provide a 'between' method failing if the number of calls is below a range" in {
       expect { between(2, 4).of(list).size }
       list.size
    } 
    "provide a 'between' method failing if the number of calls is above a range" in {
       expect { between(1, 2).of(list).size }
       3 times {i => list.size()}
    }
    "provide an 'atMost' method failing if more calls are being made" in {
       expect { atMost(2).of(list).size }
       3 times {i => list.size()}
    } 
    "provide an 'allowing' method failing if calls dont match a method pattern" in {
       expect { allowing(anything).method(withName("toSize")) }
       list.size
    } 
    "provide a 'never' method failing if any call is made to the mock" in {
       expect { never(list) }
       list.size
    } 
    "provide an equal matcher failing if the passed parameter is not equal to the specified one" in {
      expect { 1.of(list).get(equal(0)) }
      list.get(1)
    } 
    "provide a then method failing if calls are not made in sequence" in {
      expect { 
        one(list).size then 
        one(list).get(anyInt) then 
        one(list).isEmpty
      }
      list.size
      list.isEmpty
      list.get(0)
    } 
    "provide a when/set methods failing if method calls don't occur in proper states" in {
      val readiness = state("readiness")
      readiness.startsAs("not ready")
      expect { 
        allowing(list).get(anyInt) when readiness.is("ready")
      }
      list.get(0)
    } 
  }
}
trait BadMocked extends Mocked {
  var checkAfterTest = true
  override def executeTest(t: => Any) = {
    try {
      t
    } catch {
      case e: org.jmock.api.ExpectationError => {checkAfterTest = false}
    }
  }
  override def afterTest(ex: Example) = {
    if (checkAfterTest) 
      { context.assertIsSatisfied } must throwA(new org.jmock.api.ExpectationError("unexpected", null))
    else
      checkAfterTest = true
  }
}
trait Mocked extends Specification with JMocker with ExampleLifeCycle with ClassMocker {

  var list: java.util.List[Object] = _
  var scalaList: List[String] = Nil
  def createMocks = {
    scalaList = mock(classOf[List[String]], "scalaList")
    list = mock(classOf[java.util.List[Object]])
  }
  override def beforeTest(ex: Example) = createMocks
}

