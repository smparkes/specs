package scala.specs.samples

import scala.specs.runner._

object stackSuite extends JUnit3(stackSpecification)
object stackSpecification extends Specification with Sugar {
  "A stack" isSpecifiedBy (EmptyStackSpec, FullStackSpec, NotFullStackSpec)
}
trait NonEmptyStackSpec extends Specification {
  val stack = new LimitedStack[Int](10)
  var lastItemAdded = 0
  def nonEmptyStackExamples =  {
    "not be empty" in {
      stack verifies {!_.isEmpty}
    }
    "return the top item when sent #top" in {
      stack.top mustBe lastItemAdded
    }
    "not remove the top item when sent #top" in {
      stack.top mustBe lastItemAdded
      stack.top mustBe lastItemAdded
    }
    "remove the top item when sent #pop" in {
      stack.pop mustBe lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe lastItemAdded
    }
    "return the top item when sent #pop" in {
      stack.pop mustBe lastItemAdded
    }
  }
}

object FullStackSpec extends NonEmptyStackSpec {
  def createStack = { stack.clear; for (i <- 1 to 10) stack += i; lastItemAdded = stack.top }
  "A full stack" should {
    usingBefore {() => createStack }
    "behave like a non-empty stack" in nonEmptyStackExamples 
    "complain when sent #push" in {
      stack.push(11) must throwA(new Error)
    }
  }
}
object NotFullStackSpec extends NonEmptyStackSpec {
  def createStack = { stack.clear; for (i <- 1 to 3) stack += i; lastItemAdded = stack.top }
  "A non full Stack" should {
    usingBefore {() => createStack }
    "behave like a non-empty stack" in nonEmptyStackExamples 
    "add to the top when sent #push" in {
      stack push 3
      stack.top mustBe 3
    }
  }
}
object EmptyStackSpec extends Specification {
  def stack = new LimitedStack(10)
  "An empty stack" should {
    "complain when sent #top" in {
      {stack.top; ()} must throwA(new NoSuchElementException)
    }
    "complain when sent #pop" in {
      {stack.pop; ()} must throwA(new NoSuchElementException)
    }
  }
}

class LimitedStack[T](capacity: Int) extends scala.collection.mutable.Stack[T] {
  override def push(a: T*) = {
    if (size >= capacity) throw new Error("full stack") else this ++= a
  }
}