package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._
import scala.collection.immutable.HashMap

trait ScalaCheckMatchers {
  val (minSize, maxSize, maxDiscarded, minTestsOk, verbose) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk, 'verbose)
  def print(p: (Symbol, Int)*): Map[Symbol, Int] = setParams(p)(verbose) = 1
  def set(p: (Symbol, Int)*): Map[Symbol, Int] = setParams(p)
  def setParams(p: Seq[(Symbol, Int)]): Map[Symbol, Int] = {
    var params: Map[Symbol, Int] = new HashMap[Symbol, Int]
    for ((s, i) <- p) params = params + s->i
    params.withDefault(defaultParameters(_))
  }
  implicit def defaultParameters: Map[Symbol, Int] = Map(minTestsOk->0, maxDiscarded->100, minSize->0, maxSize->500, verbose->0) 
  def pass[T](g: Gen[T])(implicit p: Map[Symbol, Int]) = make[T => Boolean]((f: T => Boolean) => checkPropertyWithParameters(g)(f)(p))
  def pass[T](f: T => Boolean)(implicit p: Map[Symbol, Int]) = make[Gen[T]]((g: Gen[T]) => checkPropertyWithParameters(g)(f)(p))

  def checkPropertyWithParameters[T](g: Gen[T])(f: T => Boolean)(p: Map[Symbol, Int]) = {
    if (p(verbose) == 0)
      checkProperty[T](g)(f)(Test.Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand), false)
    else
      checkProperty[T](g)(f)(Test.Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand), true)
  }
  def checkProperty[T](g: Gen[T])(f: T => Boolean)(params: Test.Params, verbose: Boolean) = {
    val prop = forAll(g)((a: T) => { if (f(a)) proved else falsified })

    val stats = if (verbose) check(params, prop, printPropEval) else check(params, prop)  
    if (verbose){
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
