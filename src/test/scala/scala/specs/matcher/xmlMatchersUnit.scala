package scala.specs.matcher
import scala.specs.runner._
import junit.framework._

class MyTest extends TestCase

object xmlMatchersSuite extends JUnit3(xmlMatchersUnit)
object xmlMatchersUnit extends MatchersSpecification with XmlMatchers {
  "A \\\\ matcher" should {
    "be ok when a node is matched with itself" in {
      <s></s> must \\(<s></s>)
    }
    "be ok if a node is contained in another one" in {
      <a><s></s></a> must \\(<s></s>)
    }
    "be ok if a node and its children are contained in another one" in {
      <a><s><c></c></s></a> must \\(<s><s><c></c></s></s>)
    }
  }
  
  "A \\ matcher" should {
    "be ko when a node is matched with itself" in {
      <s></s> must \(<s></s>).not
    }
    "be ok if a node is contained in another one" in {
      <a><s></s></a> must \(<s></s>)
    }
    "be ok if a node and its children are contained in another one" in {
      <a><s><c></c></s></a> must \(<s><s><c></c></s></s>)
    }
  }
}
