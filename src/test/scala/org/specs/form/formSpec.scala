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
package org.specs.form

import org.specs.util._
import matcher.Matcher
import org.specs._
import org.specs.runner.{JUnitSuiteRunner, JUnit}
import samples.PersonForms
import scala.xml._
import org.specs.Sugar._
import org.specs.specification._
import org.junit.runner.RunWith

class formSpec extends HtmlSpecification with PersonForms with JUnit { persons =>
  val address = Address(37, "Nando-cho")
  val person = Person("Eric", "Torreborre", address, List("Jerome", "Olivier"))

  "A form" should {
    "have a title" in {
      val form = PersonForm("person", person)
      form.title must_== "person"
    }
    "have a default title built from the class name" in {
      class MyFormClass extends Form(this)
      new MyFormClass().title must_== "My form class"
    }
    "have a toString method returning the title of the form and the list of properties" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric")
        lastName("Torreborre")
      }
      form.toString must include("Person")
      form.firstName.toString must include("First Name: Eric")
    }
    "report its issues in xml when executed" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      (form.execute.toXhtml \\("td"))(10) must ==/(<td class="failure" valign="top"><b>TE</b>'ET' is not equal ignoring case to 'TE'</td>)
    }
  }
  "A form" can {
    "have a labelled property" in {
      val form = new PersonForm(person) {
        firstName("Eric")
      }
      form.firstName.label must_== "First Name"
      form.firstName.get must_== "Eric"
    }
    "embedded another following form as if it was a property" in {
      val form = new PersonForm("person", person) {
        tr(firstName("Eric"), new AddressForm("home", persons.address) {
                                    number(37)
                                    street("Nando-cho")})
        lastName("Torreborre")
      }
      form.toString must include("Home")
    }
    "be executed" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric")
        lastName("Torreborre")
        initials("ET")
      }
      form.execute.isOk must beTrue
    }
    "be executed and report a failure if there is one" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      form.execute.isOk must beFalse
    }
  }
  "A form when translated to xml" should {
    "translate its title to a row with a header" in {
      val form = new PersonForm("Customer", person)
      form.toXhtml must \\(<th/>) \(Text("Customer"))
    }
    "translate a property to a row" in {
      val form = new PersonForm("Customer", person) { firstName("Eric") }
      form.toXhtml must \\(<tr/>)
      form.toXhtml must \\(<td>First Name</td>)
      form.toXhtml must \\(<td colspan="3">Eric</td>)
    }
    "translate an embedded form to a nested table" in {
      val form = new PersonForm("Customer", person) { tr { new AddressForm(persons.address) { number(37) } } }
      form.toXhtml must \\(<table/>) \\(<table/>)
      form.toXhtml must \\(<tr/>) \\(<td>Number</td>)
      form.toXhtml must \\(<tr/>) \\(<td colspan="2">37</td>)
    }
    "have its title spanning all columns" in {
      val form = new PersonForm("Customer", person) { firstName("Eric") }
      form.toXhtml must \\(<th>Customer</th>, Map("colspan"->"4"))
    }
    "have its title spanning all columns - one column, 2 properties" in {
      val form = new PersonForm("Customer", person) { firstName("Eric"); lastName("T") }
      form.toXhtml must \\(<th>Customer</th>, Map("colspan"->"4"))
    }
    "have its title spanning all columns - 2 columns, 2 properties" in {
      val form = new PersonForm("Customer", person) { tr(firstName("Eric"), lastName("T")) }
      form.toXhtml must \\(<th>Customer</th>, Map("colspan"->"4"))
    }
  }
}
