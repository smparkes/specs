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
import org.specs.Sugar._
import org.specs.util._
import org.specs.runner._

trait LiterateSpecRules extends LiterateSpecification with AllProperties with Wiki {

   object example1 extends LiterateSpecification  {
     <text>{"1 must be 1" in {1 must_== 1}}</text> isSus  }
   object example2 extends LiterateSpecification  {
     <wiki>In this example <ex>*1 must be 1*</ex> { 1 must_== 1  } </wiki> isSus  }
   object example3 extends LiterateSpecification  {
     <html><ex><i>this example is not yet implemented</i></ex> { notImplemented }</html> isSus  }
   object example4 extends LiterateSpecification  {
     <text>
     <ex tags="included">this example is included</ex> { 1 must_== 1 }
     <ex>this example is not included</ex> { 1 must_== 0 }
     </text> isSus  }

   def exampleOk = checkSuccess(example1)
   def taggedExample = checkSuccess(example2)
   def notImplementedExample = checkSkipped(example3)
   def checkSuccess(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.failures).size aka "the number of failures" must_== 0
   }
   def checkSkipped(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.skipped).size aka "the number of skipped" must_== 1
   }
   def desc(s: Specification) = new HtmlRunner().formattedDescription(s.systems.first).get.toString aka "the formatted description"
   def isText = desc(example1) must include("1 must be 1")
   def isWiki = desc(example2) must include("<strong>1 must be 1</strong>")
   def isHtml = desc(example3) must include("<i>this example is not yet implemented</i>")
   def taggedExamples = {
     example4.successes.size aka "the number of successes" must_== 1
   }
   import org.specs.util.AllProperties._

   def hello(name: String): String = "hello " + name
}