package org.specs.samples
import org.specs.runner._

class stackTest extends JUnit4(stackSpecification)
object stackSpecification extends StackSpec {
  "An empty stack" should { 
    createEmptyStack.before
    "throw an exception when sent #top" in {
      stack.top must throwA(new NoSuchElementException)
    }
    "throw an exception when sent #pop" in {
      stack.pop must throwA(new NoSuchElementException)
    }
  }
  "A non-empty stack below full capacity" should {
    createNonEmptyStack.before
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
  "A stack below full capacity" should {
    createBelowCapacityStack.before
    behave like "A non-empty stack below full capacity" 
    "add to the top when sent #push" in {
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack" should { 
    createFullStack.before
    behave like "A non-empty stack below full capacity" 
    "throw an exception when sent #push" in {
      stack.push(11) must throwA(new Error)
    }
  }
}
class StackSpec extends Specification {
  val stack = new LimitedStack[Int](10)
  var lastItemAdded = 0
  def createStack(itemsNb: Int) = { stack.clear; for (i <- 1 to itemsNb) stack += i; lastItemAdded = stack.top } 
  def createEmptyStack = stack.clear
  def createNonEmptyStack = createStack(3)
  def createBelowCapacityStack = createStack(3)
  def createFullStack = createStack(stack.capacity)
}

class LimitedStack[T](val capacity: Int) extends scala.collection.mutable.Stack[T] {
  override def push(a: T*) = {
    if (size >= capacity) throw new Error("full stack") else this ++= a
  }
}