package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._

trait ScalaCheckMatchers extends AnyMatchers {
  def pass[T](f: T => Boolean) = make[Gen[T]]((g: Gen[T]) => checkProperty(g)(f))
  def pass[T](g: Gen[T]) = make[T => Boolean]((f: T => Boolean) => checkProperty(g)(f))

  def checkProperty[T](g: Gen[T])(f: T => Boolean) = {
    val prop = forAll(g)((a: T) => { if (f(a)) proved else falsified })
    check(Test.defaultParams, prop) match {
      case Stats(PropException(List((msg, _)), FailureException(ex)), tries, _) => (false, "", "A counter-example is '"+msg.toString+"': " + ex) 
      case Stats(_, tries, _) => (true, "The property passed without any counter-example after "+tries+" tries", "") 
    }
  }
}
