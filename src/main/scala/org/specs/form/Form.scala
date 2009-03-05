package org.specs.form

import matcher.{HaveTheSameElementsAs, BeEqualTo, Matcher, Matchers}
import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.specification._
import org.specs._
/**
 * This trait defines Forms which are used to group and display Props properties together.
 *
 * The purpose of forms is to provide an easy notation to display properties associated
 * to the objects under test, to set expectations, execute and display the results as an Html table
 * expectations on them.
 *
 * @org.specs.samples.formsSpecs for an example.
 * Forms output their content in XHTML in order to be included in the output of Literate Specifications.
 * The Layout trait provides simple facilities to declare the layout of the form elements
 * inside an Html table, like aligning them on a row.
 *
 */
trait Forms {
  case class Form(title: String, factory: ExpectableFactory) extends DelegatedExpectableFactory(factory)
        with DefaultExecutable with Linkable[Form] with ToHtml with Layoutable {
    type FormProperty = Executable with Linkable[_] with HasResults with ToHtml
    protected val properties: ListBuffer[FormProperty] = new ListBuffer

    /**
     * add a Prop to the Form.
     */
    def add(p: FormProperty): this.type = { properties.append(p); this }
    /**
     * add a Prop to the Form.
     */
    def set(f: this.type => Any): this.type = { f(this); this }
    /**
     * factory method for creating a property linked to an actual value.
     * Using this method adds the property to the Form
     */
    def prop[T](label: String, actual: T): MatcherProp[T] = {
      val p = Prop(label, actual, MatcherConstraint((m:Matcher[T]) => actual must m))
      add(p)
      p
    }

    /**
     * factory method for creating an iterable property linked to an actual value.
     *
     * The default matcher for an iterable properties is "haveTheSameElementsAs"
     * Using this method adds the property to the Form.
     */
    def propIterable[T](label: String, actual: Iterable[T]): MatcherPropIterable[T] = {
      val matcherConstraint: MatcherConstraint[Iterable[T]] = new MatcherConstraint[Iterable[T]](m => actual must m)
      val p = PropIterable(label, actual, matcherConstraint)
      p.matchesWith(new HaveTheSameElementsAs(_))
      add(p)
      p
    }
    /**
     * add a subForm to this form.
     */
    def form[F <: Form](f: F): F = {
      add(f)
      f
    }

    /** executing the Form is done by executing all of its properties. */
    def executeThis = properties.foreach(_.execute)

    /** the Form failures is the executing the Form is done by executing all of its properties. */
    override def failures = properties.flatMap(_.failures)
    override def skipped = properties.flatMap(_.skipped)
    override def errors = properties.flatMap(_.errors)

    override def toString = {
      title +
      properties.filter(_.previous.isEmpty).mkString("\n  ", "\n  ", "")
    }
    override def toEmbeddedHtml = <td>{toHtml}</td>
    override def toHtml = {
      updateLastTd(<table class="dataTable"><tr><th>{title}</th></tr>{ if (!xml.isEmpty) xml else properties.map(inRow(_)) }</table>)
    }
  }
}