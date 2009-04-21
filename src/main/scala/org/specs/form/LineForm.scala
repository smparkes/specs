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
import scala.collection.mutable._
import scala.xml._
import org.specs.xml.NodeFunctions._

/**
 * A LineForm is a set of LineProps or LineFields which are displayed on the same line.
 * 
 * It is meant to be used in a SeqForm or a SetForm.
 * 
 * The field and prop methods are overriden to create and attach LineFields and LineProps to the Form
 *
 */
class LineForm extends Form {

  /** list of the properties to display */
  protected val lineProperties: ListBuffer[LabeledXhtml] = new ListBuffer

  /** add a new LineField to that line */
  override def field[T](label: String, actual: =>T) = {
    val f = new LineField(label, actual)
    lineProperties.append(f)
    f
  }
  /** add a new LineProp to that line */
  override def prop[T](label: String, actual: =>T) = {
    val p = new LineProp(label, None, Some(actual), Some(MatcherConstraint((m:org.specs.matcher.Matcher[T]) => actual must m)))
    lineProperties.append(p)
    add(p)
    p
  }
  /** when rows are requested (one row only in that case), the properties are added on the same row.  */
  override def rows = {
    tr(lineProperties:_*)
    super.rows
  }
  override def propertiesAndFields = lineProperties.toList ::: super.propertiesAndFields
  /** extract a header from all the property labels */
  def header = reduce(lineProperties.map(_.label), { (cur: String) => <th>{cur}</th> })
  /** return the xhtml of all properties without the label (because they are LineProp and LineField) */
  override def toXhtml = reduce(lineProperties, { (p: LabeledXhtml) => p.toXhtml })
}
object LineForm {
  /** create a LineForm with labels only to create header rows */
  def apply(labels: String*) = new LineForm {
    labels.foreach((s:String) => field(s, s))
  }
}