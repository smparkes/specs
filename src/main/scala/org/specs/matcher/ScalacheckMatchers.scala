package org.specs.matcher
import scalacheck._
import scalacheck.Gen
import scalacheck.Prop
import scalacheck.Prop._
import scalacheck.Test
import scalacheck.Test._
import scalacheck.ConsoleReporter._
import scala.collection.immutable.HashMap
import org.specs.io.ConsoleOutput
import org.specs.matcher.ScalacheckParameters._
import org.specs.matcher.MatcherUtils._
import org.specs.Sugar._
import org.specs.specification._

/**
 * The <code>ScalacheckMatchers</code> trait provides matchers which allow to 
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">Scalacheck project</a>
 */
trait ScalacheckMatchers extends ConsoleOutput with ScalacheckFunctions {
   /**
    * default parameters. Uses Scalacheck default values and doesn't print to the console
    */
   implicit def defaultParameters = new Parameters(ScalacheckParameters.setParams(Nil))
		
   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>function must pass(generated_values)</code><br>
    * @param params are the given by the implicit default parameters of Scalacheck
    */
   def pass[T](g: Gen[T])(implicit params: Parameters) = new Matcher[T => Boolean](){
      def apply(f: => (T => Boolean)) = checkFunction(g)(f)(params)
    }

   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>generated_values must pass(function)</code>
    */
   def pass[T](f: T => Boolean)(implicit params: Parameters) = new Matcher[Gen[T]](){
      def apply(g: => Gen[T]) = checkFunction(g)(f)(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>generated_values must pass(property)</code>
    */
   def pass[T](prop: Prop)(implicit params: Parameters) = new Matcher[Gen[T]](){
     def apply(g: => Gen[T]) = checkProperty(forAll(g)(a => prop))(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>property must pass</code>
    */
    def pass(implicit params: Parameters) = new Matcher[Prop](){
     def apply(p: => Prop) = checkProperty(p)(params)
    }

   def checkFunction[T](g: Gen[T])(f: T => Boolean)(p: Parameters) = {
      // create a scalacheck property which states that the function must return true
      // for each generated value
      val prop = forAll(g)(a => if (f(a)) proved else falsified)
      checkProperty(prop)(p)
   }
   /**
    * checks if the property is true for each generated value, and with the specified
    * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
    * and indicates if the generation should be verbose or not 
    */
   def checkProperty(prop: Prop)(p: Parameters) = {
     checkScalacheckProperty(prop)(Test.Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand), p.verbose)
   }
    
  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  def checkScalacheckProperty(prop: Prop)(params: Test.Params, verbose: Boolean) = {
     // will print the result of each test if verbose = true
     def printResult(result: Option[Prop.Result], succeeded: Int, discarded: Int): Unit = {
			 if (!verbose) return
       result match {
         case None => ()
         case Some(r) => printf("\rTested: {0}", r.args) 
       }
       
       if (discarded == 0) 
         printf("\rPassed {0} tests", succeeded)
       else 
         printf("\rPassed {0} tests; {1} discarded", succeeded, discarded)
       flush
     }
     
     // check the property
     val stats = check(params, prop, printResult) 
     
     // display the final result if verbose = true
     if (verbose) {
       val s = prettyTestStats(stats)
       printf("\r{0} {1}{2}\n", if (stats.result.passed) "+" else "!", s, List.make(70 - s.length, " ").mkString(""))
     }

     // depending on the result, return the appropriate success status and messages
     // the failure message indicates a counter-example to the property
     def afterNTries(n: Int) = "after " + (if (n <= 1) n + " try" else n + " tries")
     def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)
     def afterNShrinks(shrinks: Int) = if (shrinks >= 1) (", " + shrinks + " shrinks") else ""

     stats match {
       case Test.Stats(Passed(), n, _)          => (true,  noCounterExample(n), "A counter-example was found " + afterNTries(n)) 
       case s@Test.Stats(GenException(e), n, _) => (false, noCounterExample(n), prettyTestStats(s)) 
       case s@Test.Stats(Exhausted(), n, _)     => (false, noCounterExample(n), prettyTestStats(s)) 
       case Test.Stats(Failed(List(Arg(_, msg, shrinks))), n, _) => 
         (false, noCounterExample(n), "A counter-example is '"+msg+"' (" + afterNTries(n) + afterNShrinks(shrinks) + ")") 
       case Test.Stats(Failed(Arg(_, msg, shrinks)::_), n, _) => 
         (false, noCounterExample(n), "A counter-example is '"+msg+"' (" + afterNTries(n) + afterNShrinks(shrinks) + ")") 
       case Test.Stats(PropException(List(Arg(_, msg, shrinks)), FailureException(ex)), n, _) => 
         (false, noCounterExample(n), "A counter-example is '"+msg+"': " + ex + " ("+afterNTries(n)+")") 
       case s@Test.Stats(PropException(List(Arg(_, msg, shrinks)), ex), n, _) => 
         (false, noCounterExample(n), prettyTestStats(s)) 
     }
   }
  
}
/**
 * This trait is used to facilitate testing by mocking Scalacheck functionalities
 */
trait ScalacheckFunctions {
  def check(params: Test.Params, prop: Prop, printResult: (Option[Prop.Result], Int, Int) => Unit) = Test.check(params, prop, printResult)
  def forAll[A,P](g: Gen[A])(f: A => Prop): Prop = Prop.forAll(g)(f)
}
/**
 * This trait provides generation parameters to use with the <code>ScalacheckMatchers</code>
 */
trait ScalacheckParameters {
  /**
   * Values which can be used as Symbol aliases to specify Scalacheck parameters<br>
   * The naming is a bit different, in order to keep short names for frequent use cases<ul>
   *  <code><li>minTestsOk == minSuccessfulTests
   *  <li>maxDiscarded == maxDiscardedTests
   *  <li>minSize and maxSize keep their name <code><ul>
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk)

  /**
   * Default values for Scalacheck parameters
	 */
  def defaultValues = Map(minTestsOk->100, maxDiscarded->500, minSize->0, maxSize->100) 

  /**
   * This class is the base class for the print and set case classes.<br>
   * It contains a Map of generation parameters and indicates if the generation
   * must be verbose.
   */  
  sealed class Parameters(params: Map[Symbol, Int]) {
    def apply(s: Symbol) = params(s)
    def verbose = false
  }

  /**
   * This class is used to set parameters and to print the property evaluation on the console<br>
   * Usage: <pre><code>
   *  generated_values must pass { v =>
   *    property(v) mustBe ok
   *  }(print(minTestsOk->15, maxDiscarded->20))</code></pre> 
   */  
  case class display(p: (Symbol, Int)*) extends Parameters(setParams(p)) {
    override def verbose = true
  }

  /**
   * This class is used to set parameters but nothing will be printed to the console<br>
   * Usage: <pre><code>
   * generated_values must pass { v =>
   *   property(v) mustBe ok
   * }(set(minTestsOk->15, maxDiscarded->20))</code></pre> 
   */  
  case class set(p: (Symbol, Int)*) extends Parameters(setParams(p))

  /**
   * Those parameters will print the result on the console and use the default settings<br>
   * Usage: <pre><code>
   * generated_values must pass { v =
   *   property(v) mustBe ok
   * }(display) </code></pre>
   */  
  val display = new Parameters(setParams(Nil)) {override def verbose = true}
    
  /**
   * This function transform the varargs parameters into a Map with default values
   * if some expected values are not provided by the user
   */ 
  def setParams(p: Seq[(Symbol, Int)]): Map[Symbol, Int] = {
    var params: Map[Symbol, Int] = new HashMap[Symbol, Int]
    p foreach { pair: (Symbol, Int) => 
        //  this is a useful check in case of print(null) or set(null)
        if (pair == null || pair._1 == null)
          throw new RuntimeException("null values are not accepted in scalacheck parameters: " + q(pair))
        else { 
          val (s, i) = pair
          params = params + s->i
        }
    }
    params.withDefault(defaultValues)
  }
}
/**
 * Companion object of the <code>ScalacheckParameters</code> trait<br>
 * Use import to access the <code>ScalacheckParameters</code> functions
 */
object ScalacheckParameters extends ScalacheckParameters
