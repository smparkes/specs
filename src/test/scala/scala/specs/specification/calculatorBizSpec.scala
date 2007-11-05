package scala.specs.specification
import scala.specs._
import scala.specs.specification._
import scala.specs.runner._
import scala.util.DataTables

object calcRunner extends ConsoleRunner(calcSpecificationSpec)
object calcSpecificationSpec extends BizSpecification with DataTables {
  val calc = new Object {def add(x: Int, y: int): Int = x + y }
  "A literal spec for the calculator" is 
<p>
A calculator can { "add integers: calc.add(a, b) == c" inTable { 
                     "a" 	| "b" | "c" | 
                      1	   	!  2  !  3  |
                      2     !  6  !  6  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a: Int, b: Int, c: Int) => c must_== calc.add(a, b) }
                    }
} which is the summum of technicity

</p>
  "A classical spec for the calculator" should {
    "use tables directly" in {
                     "a" 	| "b" | "c" |> 
                      1	   	!  2  !  3  |
                      2     !  6  !  6  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a: Int, b: Int, c: Int) => c must_== calc.add(a, b) }
   }
 }
}

