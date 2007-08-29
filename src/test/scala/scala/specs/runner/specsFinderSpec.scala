package scala.specs
import scala.io.mock.MockFileSystem
import scala.io.ConsoleOutput
import scala.specs.integration._

object specsFinderSuite extends JUnit3(specsFinderSpec)
object specsFinderSpec extends Specification with Init {
  
  "A specs finder" should {
    "find the name of the specification in a specification file" in {
       finder.addFile("file1", packageDeclaration + specificationContent)
       finder.specificationNames("file1") mustExistMatch "trueSpec"
    }
    "find the name of the specification with its package name in specification file" in {
      finder.addFile("file1", packageDeclaration + specificationContent)
      finder.specificationNames("file1") mustContain "scala.specs.trueSpec"
    }
    "return a proper specification name for a package declaration ending with a;" in {
      finder.addFile("file1", packageDeclarationWithSc + specificationContent)
      finder.specificationNames("file1") mustContain "scala.specs.trueSpec"
    }
    "return an empty list if there is no specification declaration found in the file" in {
      finder.addFile("file1", packageDeclaration)
      finder.specificationNames("file1") must_== List()
    }
    "return a list with 2 specification names if the file contains 2 specs" in {
      finder.addFile("file1", packageDeclaration + specificationContent + specificationContent)
      finder.specificationNames("file1") must_== List("scala.specs.trueSpec", "scala.specs.trueSpec")
    }
    "return a list with 2 specification names if run over a directory with 2 files" in {
      finder.addFile("file1", packageDeclaration + specificationContent)
      finder.addFile("file2", packageDeclaration + specificationContent)
      finder.specificationNames("dir1") must_== List("scala.specs.trueSpec", "scala.specs.trueSpec")
    }
  }
  object finder extends MockFileSystem with SpecsFinder with ConsoleOutput
}
trait Init {
  val packageDeclaration = "package scala.specs"
  val packageDeclarationWithSc = packageDeclaration + ";"
  val specificationContent = """
    object trueSpec extends Specification with MockOutput {
      "A specification" should {
        "have example 1 ok" in { true }
        }
      }
    """
}

