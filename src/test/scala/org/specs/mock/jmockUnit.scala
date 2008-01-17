package org.specs.mock
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.Products._
import org.specs.specification._
import org.specs.runner._
import org.hamcrest.core._
import org.specs.matcher._

class jmockUnitTest extends JUnit3(jmockUnit)
object jmockUnitRunner extends ConsoleRunner(jmockUnit)
object jmockUnit extends Specification {
  "JMock unit tests".areSpecifiedBy(jmockGoodUnit, jmockBadUnit)
}
object jmockGoodUnit extends Mocked {
  "The jmocker" should {
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
    "provide an 'allowingMethodsLike' method succeeding if calls match a method pattern" in {
      expect { allowingMethodsLike("size") }
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
    "provide an 'ignoringMethodsLike' method accepting any call matching a method pattern and returning default values" in {
      expect { ignoringMethodsLike("size") }
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
      expect { 1.of(list).get(will(be_==(0))) }
      list.get(0)
    } 
    "provide a willReturn method to specify the value which must be returned" in {
      expect { 1.of(list).get(will(be_==(0))) willReturn "new" }
      list.get(0) must_== "new"
    } 
    "provide a willThrow method to specify the exception which must be thrown" in {
      expect { 1.of(list).get(will(be_==(0))) willThrow new java.lang.Exception("ouch") }
      list.get(0) must throwA(new Exception)
    } 
    "provide a willReturnIterator method to specify the a returned iterator" in {
      val expected = List[String]("hey")
      expect { 1.of(scalaList).elements willReturnIterator expected }
      scalaList.elements.next must_== "hey"
    } 
    "provide a willReturnIterable method to specify a returned iterable" in {
      expect { 1.of(scalaList).take(anyInt) willReturnIterable List("hey") }
      val matcher: Matcher[Iterable[String]] = be_==(List("hey"))
      scalaList.take(1) must(matcher)
    }
    "provide a willReturn method accepting a block to return another mock and specify it too" in {
      case class Module(name: String)
      case class Project(modules: List[Module], name: String)
      case class Workspace(project: Project)
      val workspace = mock(classOf[Workspace])
 
      expect { 
        1.of(workspace).project.willReturn(willBe(classOf[Project]){
p: Project => 
"here".pln;p.pln
one(p).name willReturn "hi"
})
      }
      val p = workspace.project
      println(workspace.project.name)
    } 
  }
}
object jmockBadUnit extends BadMocked {
  "The jmocker" should {
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

