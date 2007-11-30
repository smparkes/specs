package scala.util
import scala.specs.Specification
import scala.specs.runner._

object dataRowTest extends JUnit3(dataRowUnit)
object dataRowUnit extends Specification {
  "a data row" should {
    "print out its values separated by |" in {
      val datarow = DataRow3[Int, Int, Int](1, 2, 3)
      datarow.toString must_== "|1|2|3|"
    }
  }
}
