package org.specs.runner
import org.specs._
import org.specs.util._
import org.specs.runner._
import org.specs.Sugar._

object htmlRunnerUnit extends Specification with DataTables {
  lazy val table = "a"    | "b"  | "result" |
                     1    !  1   ! 2        |
                     1    !  1   ! 2        |
                     3    !  1   ! 5        | { (a: Int, b: Int, c: Int) => 
                    a + b must_== c  }

  "the xmlFor function" should { doBefore { executeTable }
    "create an html table for a DataTable" in {
     //  runner.xmlFor(table) must \\(<table></table>) 
    }
    "create a header for the DataTable" in {
    //  runner.xmlFor(table) must (\\(<td>a</td>) and \\(<td>b</td>) and \\(<td>c</td>))
    }
  }
  def executeTable = try { table.execute } catch { case _ => }

}
object runner extends HtmlRunner(null)
class htmlRunnerUnitTest extends JUnit4(htmlRunnerUnit)