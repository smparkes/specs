package scala.io
import scala.specs._
import scala.specs.runner._
import scala.io.mock._

object fileWriterSuite extends JUnit3(fileWriterSpec)
object fileWriterSpec extends Specification {
  "A FileWriter" should {
    "write inside a file" in {
      fw.write("filePath"){ file => 
        file.write("hello world")
      }
      out.messages mustContain "hello world"
    }
    "close the file if an exception occurs and rethrow the exception" in {
      try {
        fw.write("filePath"){ file => 
          throw new Error("bad")
        }
      } catch {case e => { e.getMessage mustBe "bad"}}
      out.closed mustBe true
    }
  }
  object fw extends FileWriter { 
    override def getWriter(path: String) = out
  }
  object out extends MockWriter
}
