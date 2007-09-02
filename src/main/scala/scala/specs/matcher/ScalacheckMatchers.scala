package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._

trait ScalaCheckMatchers extends AnyMatchers {
  implicit def defaultParameters = Test.defaultParams 
  def pass[T](f: T => Boolean)(implicit p: Test.Params) = make[Gen[T]]((g: Gen[T]) => checkProperty(g)(f)(p))
  def pass[T](g: Gen[T])(implicit p: Test.Params) = make[T => Boolean]((f: T => Boolean) => checkProperty(g)(f)(p))

  def checkProperty[T](g: Gen[T])(f: T => Boolean)(params: Test.Params) = {
    val prop = forAll(g)((a: T) => { if (f(a)) proved else falsified })
    val stats = if (params != Test.defaultParams) check(params, prop, printPropEval) else check(params, prop) 
    if (params != Test.defaultParams) {
      val s = stats.pretty
      printf("\r{2} {0}{1}\n", s, List.make(70 - s.length, " ").mkString(""), 
          if(stats.result.passed) "+" else "!")
    }
    stats match {
      case Stats(PropException(List((msg, _)), FailureException(ex)), tries, _) => (false, "", "A counter-example is '"+msg.toString+"': " + ex) 
      case Stats(_, tries, _) => (true, "The property passed without any counter-example after "+tries+" tries", "") 
    }

  }

  def printPropEval(res: Option[Prop.Result], succeeded: Int, discarded: Int) = {
    if(discarded == 0) printf("\rPassed {0} tests",succeeded)
    else printf("\rPassed {0} tests; {1} discarded",succeeded,discarded)
    Console.flush
  }
}
