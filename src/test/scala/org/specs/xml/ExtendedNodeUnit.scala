package org.specs.xml
import org.specs._
import org.specs.runner._
import org.specs.xml.ExtendedNode._
import org.specs.xml.NodeFunctions._
import scala.xml.NodeSeq._
import scala.xml._

class extendedNodeUnit extends Specification with JUnit {
  "An isSpaceNode function" should {
    "return false for a node with a simple label" in {
      <a/>.isSpaceNode mustBe false
    }
    "return true for a node containing space" in {
      <a> </a>.child.last.isSpaceNode mustBe true
    }
    "return true for a node containing a newline and spaces" in {
      <a>
        </a>.child.last.isSpaceNode mustBe true
    }
  }

  "An isEqualIgnoringSpace function" should {
    "return true for <a> ==/ <a>" in {
      <a/> ==/ <a/> mustBe true
    }
    "return true for <a></a> ==/ <a></a>" in {
      <a></a> ==/ <a></a> mustBe true
    }
    "return true for <a> </a> ==/ <a></a>" in {
      <a> </a> ==/ <a></a> mustBe true
    }
    "return true for <b/><c> </c> ==/ <b/><c></c>" in {
      fromSeq(<a><b/><c> </c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList) mustBe true
    }
    "return false for <b/><c>1</c> ==/ <b/><c></c>" in {
      fromSeq(<a><b/><c>1</c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList) mustBe false
    }
    "return true for <a>\n</a> ==/ <a></a>" in {
      <a>
      </a> ==/ <a></a> mustBe true
    }
    "return true for unordered sequences of nodes <a><b/><c/></a> ==/ <a><c/><b/></a>" in {
      <a><b/><c/></a> ==/ <a><c/><b/></a> must beTrue
    }
    "return false for <a>1</a> ==/ <a></a>" in {
      <a>1</a> ==/ <a></a> mustBe false
    }
    "return true for Text(1) ==/ Text( 1 )" in {
      Text("1").isEqualIgnoringSpace(Text(" 1 ")) must beTrue
    }
    "return false for Text(1) ==/ Text(2)" in {
      Text("1").isEqualIgnoringSpace(Text("2")) must beFalse
    }
  }
  "An isEqualIgnoringSpaceOrdered function" should {
    "return true for <a><b/><c/></a> ==/ <a><b/><c/></a>" in {
      <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><b/><c/></a>) must beTrue
    }
    "return false for <a><b/><c/></a> ==/ <a><c/><d/></a>" in {
      <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><c/><b/></a>) must beFalse
    }
  }
}
