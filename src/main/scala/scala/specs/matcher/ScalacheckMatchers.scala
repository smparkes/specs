package scala.specs.matcher
import scalacheck._
import scalacheck.Gen
import scalacheck.Prop
import scalacheck.Test
import scala.collection.immutable.HashMap
import scala.io.ConsoleOutput
import scala.specs.matcher.ScalacheckParameters._
import scala.specs.matcher.MatcherUtils._
import scala.specs.Sugar._

/**
 * The <code>ScalacheckMatchers</code> trait provides matchers which allow to 
 * assess properties multiple times with generated data
 */
trait ScalacheckMatchers extends ConsoleOutput with ScalacheckFunctions {
   /**
    * Default parameters. Use Scalacheck default values and don't print to the console
    */
   implicit def defaultParameters = new Parameters(ScalacheckParameters.setParams(Nil))
		
   /**
    * Matches ok if the <code>property T => Boolean</code> returns <code>true</code> for any generated value
    * Usage: <code>properties must pass(generated_values)</code>
    * params are the given by the implicit default parameters of Scalacheck
    */
   def pass[T](g: Gen[T])(implicit params: Parameters) = new Matcher[T => Boolean](){
      def apply(f: => (T => Boolean)) = checkPropertyWithParameters(g)(f)(params)
    }

   /**
    * Matches ok if the <code>property T => Boolean</code> returns <code>true</code> for any generated value
    * Usage: <code>generated_values must pass(property)</code>
    */
   def pass[T](f: T => Boolean)(implicit params: Parameters) = new Matcher[Gen[T]](){
      def apply(g: => Gen[T]) = checkPropertyWithParameters(g)(f)(params)
   }

   /**
    * checks if the property is true for each generated value, and with the specified
    * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
    * and indicates if the generation should be verbose or not 
    */
   def checkPropertyWithParameters[T](g: Gen[T])(f: T => Boolean)(p: Parameters) = {
     checkProperty[T](g)(f)(Test.Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand), p.verbose)
   }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  def checkProperty[T](g: Gen[T])(f: T => Boolean)(params: Test.Params, verbose: Boolean) = {
     // the 'real' scalacheck property states that the function must return true
     // for each generated value
     val prop = forAll(g)(a => { if (f(a)) Prop.proved else Prop.falsified })

     // will print the result of each test if verbose = true
     def printResult(result: Option[Prop.Result], succeeded: Int, discarded: Int): Unit = {
			 if (!verbose) return
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
       val s = stats.pretty
       printf("\r{0} {1}{2}\n", if (stats.result.passed) "+" else "!", s, List.make(70 - s.length, " ").mkString(""))
     }

     // depending on the result, return the appropriate success status and messages
     // the failure message indicates a counter-example to the property
     stats match {
     case Test.Stats(Test.Failed(List((msg, _))), tries, _) => 
         (false, "The property passed without any counter-example after "+tries+" tries", "A counter-example is '"+msg.toString+"'") 
       case Test.Stats(Test.PropException(List((msg, _)), FailureException(ex)), tries, _) => 
         (false, "The property passed without any counter-example after "+tries+" tries", "A counter-example is '"+msg.toString+"': " + ex) 
       case Test.Stats(_, tries, _) => 
         (true, "The property passed without any counter-example after "+tries+" tries", "A counter-example was found") 
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
   * Values which can be used as Symbol aliases to specify Scalacheck parameters
   * The naming is a bit different, in order to keep short names for frequent use cases
   *  minTestsOk == minSuccessfulTests
   *  maxDiscarded == maxDiscardedTests
   *  minSize and maxSize keep their name 
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk)

  /**
   * Default values for Scalacheck parameters
	 */
  def defaultValues = Map(minTestsOk->100, maxDiscarded->500, minSize->0, maxSize->100) 

  /**
   * This class is the base class for the print and set case classes
   * it contains a Map of generation parameters and indicates if the generation
   * must be verbose
   */  
  sealed class Parameters(params: Map[Symbol, Int]) {
    def apply(s: Symbol) = params(s)
    def verbose = false
  }

  /**
   * This class is used to set parameters and to print the property evaluation on the console
   * Usage: generated_values must pass { v =>
   *            property(v) mustBe ok 
   *        }(print(minTestsOk->15, maxDiscarded->20)) 
   */  
  case class print(p: (Symbol, Int)*) extends Parameters(setParams(p)) {
    override def verbose = true
  }

  /**
   * This class is used to set parameters but nothing will be printed to the console
   * Usage: generated_values must pass { v =>
   *            property(v) mustBe ok 
   *        }(set(minTestsOk->15, maxDiscarded->20)) 
   */  
  case class set(p: (Symbol, Int)*) extends Parameters(setParams(p))

  /**
   * Those parameters will print the result on the console and use the default settings
   * Usage: generated_values must pass { v =>
   *            property(v) mustBe ok 
   *        }(print) 
   */  
  val print = new Parameters(setParams(Nil)) {override def verbose = true}
    
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
 * Companion object of the <code>ScalacheckParameters</code> trait
 * Use import to access the <code>ScalacheckParameters</code> functions
 */
object ScalacheckParameters extends ScalacheckParameters
