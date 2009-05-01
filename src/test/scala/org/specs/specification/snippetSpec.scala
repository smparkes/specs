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
import org.specs._
import org.specs.runner._
import org.specs.util.Property

class snippetSpec extends Specification with JUnit with Snippets {
  "A snippet" should {
    "have a prelude method" in {
      val s = Snippet("").prelude("prelude")
      s.code must include("prelude")
    }
    "cumulate preludes" in {
      val s = Snippet("").prelude("prelude1").prelude("prelude2")
      s.code must include("prelude1") and include("prelude2")
    }
  }
  "The Snippets trait" should {
    "allow a property to store the current snippet" in {
      val it = Property[Snippet](Snippet(""))
      "import org.specs._" prelude it
      "object s extends Specification" snip it
      it.get.code must include("import") //and include("Specification")
    }
  }
}
