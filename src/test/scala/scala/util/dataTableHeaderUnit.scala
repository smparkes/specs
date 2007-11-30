package scala.util
import scala.specs.Specification
import scala.specs.runner._

object datatableHeaderTest extends JUnit3(dataTableHeaderUnit)
object dataTableHeaderUnit extends Specification with DataTables {
  "a data table header" should {
    "print out the column names separated by |" in {
      val tableHeader = "a"|"b"|"c"|
        
      tableHeader.toString must_== "|a|b|c|"
    }
  }
}
