package scala.xml
import scala.specs._
import scala.specs.runner._
import scala.xml.ExtendedNode._
import scala.xml.NodeFunctions._

object extendedNodeSuite extends JUnit3(extendedNodeUnit)
object extendedNodeUnit extends Specification {
  "An isSpaceNode function" should {
    "return false for a node with a simple label" in {
      <a/>.isSpaceNode mustBe false  
    }
    "return true for a node containing space" in {
      <a> </a>.child.toList.head.isSpaceNode mustBe true  
    }
  }
  
  "An isEqualIgnoreSpace function" should {
    "return true for <a> ==/ <a>" in {
      <a/> ==/ <a/> mustBe true
    }
    "return true for <a></a> ==/ <a></a>" in {
      <a></a> ==/ <a></a> mustBe true
    }
    "return true for <a> </a> ==/ <a></a>" in {
      <a> </a> ==/ <a></a> mustBe true
    }
    "return true for <a>\n</a> ==/ <a></a>" in {
      <a> 
      </a> ==/ <a></a> mustBe true
    }
    "return false for <a>1</a> ==/ <a></a>" in {
      <a>1</a> ==/ <a></a> mustBe false
    }
  }
}
