package org.specs.io
import org.specs._
import org.specs.runner._
import org.specs.io.mock._

class fileWriterSpec extends Specification with JUnit {
  "A FileWriter" should {
    doAfter { new java.io.File("filePath").delete }
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
