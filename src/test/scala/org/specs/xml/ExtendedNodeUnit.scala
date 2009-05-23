/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.xml
import org.specs._
import org.specs.runner._
import org.specs.xml.ExtendedNode._
import org.specs.xml.NodeFunctions._
import scala.xml.NodeSeq._
import scala.xml._
import java.lang.UnsupportedOperationException

class extendedNodeUnit extends SpecificationWithJUnit {
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
    "not fail with a Group" in {
      Group(<a/><b/>).isSpaceNode must not throwA(new UnsupportedOperationException)
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
