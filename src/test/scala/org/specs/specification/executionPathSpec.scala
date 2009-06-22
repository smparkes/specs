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
package org.specs.specification
import org.specs.util._

class executionPathSpec extends spex.Specification with Classes {
  shareVariables()
  val spec = new Specification {
    "this system" should {
      "have one ex" in {
        "with one sub11" in { 1 must_== 1 }
        "with one sub12" in { 1 must_== 1 }
      }
      "have two ex" in {
        "with one sub21" in { 1 must_== 1 }
        "with one sub22" in { 1 must_== 1 }
      }
    }
  }
  "An tree path" should {
    "have a ::: method to append 2 paths" in {
      (TreePath(List(0, 1)) ::: TreePath(List(2, 3))) must_== TreePath(List(0, 1, 2, 3)) 
    }
    "have a pathFromRoot method returning the path of the element in the tree of elements" in {
      spec.systems(0).examples(0).subExamples(0).pathFromRoot must_== TreePath(List(0, 0, 0, 0))
    }
    "have a pathFromRoot method returning the path of the element in the tree of elements" in {
      spec.systems(0).examples(1).subExamples(1).pathFromRoot must_== TreePath(List(0, 0, 1, 1))
    }
  }
  "An Example" should {
    "not create subexamples until asked for it" in {
      val e = new Example("", this)
      var spyCalled = false
      e.in { 
        spyCalled = true
        new Example("subex", this) 
      }
      spyCalled must beFalse
      e.subExamples
      spyCalled must beTrue
    }
    "have a getExample method returning the subexample at a given path" in {
      val sys = spec.systems(0)
      val ex = sys.examples(0)
      val subex = ex.subExamples(1)
      ex.getExample(TreePath(1)) must_==(Some(subex))
    }
  }
  "A Sus" should {
    "have a getExample method returning the example at a given path" in {
      val sus = spec.systems(0)
      val ex = sus.examples(1)
      sus.getExample(TreePath(1)) must_== Some(ex)
    }
  }
  "A specification" should {
    "have all activation paths set to a subexample" in {
      spec.pathFromRoot must_== TreePath(0)
      spec.systems(0).pathFromRoot must_== TreePath(List(0, 0))
      spec.systems(0).examples(0).pathFromRoot must_== TreePath(List(0, 0, 0))
      spec.systems(0).examples(0).subExamples(1).pathFromRoot must_== TreePath(List(0, 0, 0, 1))
    }
    "have a getExample method returning the example at a given path" in {
      val subex = spec.systems(0).examples(0).subExamples(1)
      spec.getExample(subex.pathFromRoot) must_== Some(subex)
    }
  }
}
