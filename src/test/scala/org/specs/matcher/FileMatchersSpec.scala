package org.specs.matcher
import org.specs.runner._
import java.io.File
import org.specs.io.mock._

class FileMatchersTest extends Runner(FileMatchersSpec) with JUnit
object FileMatchersSpec extends MatchersSpecification with MockFileSystem  {
  val existingPath = "path"
  val unexistingPath = "absent"
  addFile(existingPath, "")
  "The Path matchers" should {
    "provide an beEqualIgnoringSep matcher checking if two paths are the same regardless of their separators" in {
      "c:\\temp\\hello" must beEqualIgnoringSep("c:/temp/hello")
      assertion("c:\\temp\\hello" must beEqualIgnoringSep("c:/temp2/hello")) must failWith("'c:\\temp\\hello' is not equal ignoring separators to 'c:/temp2/hello'")
    }
    "provide an existPath / beAnExistingPath matcher to check if a file exists" in {
      existingPath must existPath
      existingPath must beAnExistingPath
      assertion(unexistingPath must beAnExistingPath) must failWith("'absent' doesn't exist")
    }
    "provide an beReadablePath matcher to check if a file can be read" in {
      setReadable(existingPath)
      existingPath must beReadablePath

      setNotReadable(existingPath)
      assertion(existingPath must beReadablePath) must failWith("'path' can't be read")
    }
    "provide an beWritablePath matcher to check if a file can be written" in {
      setWritable(existingPath)
      existingPath must beWritablePath

      setNotWritable(existingPath)
      assertion(existingPath must beWritablePath) must failWith("'path' can't be written")
    }
    "provide an beAbsolutePath matcher to check if a file is absolute" in {
      "/tmp" must beAbsolutePath
      assertion(existingPath must beAbsolutePath) must failWith("'path' is not absolute")
    }
    "provide an beHiddenPath matcher to check if a file is hidden" in {
      ".tmp" must beHiddenPath
      assertion(existingPath must beHiddenPath) must failWith("'path' is not hidden")
    }
    "provide an beFilePath matcher to check if a file is a file" in {
      "c:/tmp.txt" must beFilePath
      assertion("tmp/" must beFilePath) must failWith("'tmp/' is not a file")
    }
    "provide an beDirectoryPath matcher to check if a file is a directory" in {
      "c:/tmp/" must beDirectoryPath
      assertion("test.txt" must beDirectoryPath) must failWith("'test.txt' is not a directory")
    }
    "provide an haveName matcher to check if a file has a given name" in {
      "c:/tmp/test.txt" must havePathName("test.txt")
      assertion("c:/tmp/test.txt" must havePathName("tst.txt")) must failWith("'c:/tmp/test.txt' is not named 'tst.txt'")
    }

    skip("should be fixed later")
    "provide an havePathAbsolutePath matcher to check if a file has a given absolute path" in {
      "c:/tmp/test.txt" must havePathAbsolutePath("c:/tmp/test.txt")
      assertion("c:/tmp/test.txt" must havePathAbsolutePath("tst.txt")) must failWith("'c:/tmp/test.txt' doesn't have absolute path 'tst.txt' but 'c:/tmp/test.txt'")
    }
    "provide an havePathCanonicalPath matcher to check if a file has a given canonical path" in {
      "c:/tmp/dir/../test.txt" must havePathCanonicalPath("c:/tmp/test.txt")
      assertion("c:/tmp/dir/test.txt" must havePathCanonicalPath("c:/tmp/test.txt")) must failWith("'c:/tmp/dir/test.txt' doesn't have canonical path 'c:/tmp/test.txt' but 'c:/tmp/dir/test.txt'")
    }
    "provide an haveParentPath matcher to check if a file has a given parent path" in {
      "c:/tmp/dir/test.txt" must haveParentPath("c:/tmp")
      assertion("c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/test.txt")) must failWith("'c:/tmp/dir/test.txt' doesn't have parent path 'c:/tmp/test.txt' but 'c:/tmp/dir'")
    }
    "provide an havePathList matcher to check if a file has a given children" in {
      "c:/tmp" must havePathList(List("c:/tmp/test.txt"))
      assertion("c:/tmp" must havePathList(List("c:/tmp/test.txt"))) must failWith("'c:/tmp' doesn't have files List(c:/tmp/test.txt) but List()")
    }
  }
  "The File matchers" should {
    "provide an exist matcher to check if a file exists" in {
      new File(existingPath) must exist
    }
    "provide an beReadable matcher to check if a file can be read" in {
      setReadable(existingPath)
      new File(existingPath) must beReadable
    }
    "provide an beWritable matcher to check if a file can be written" in {
      setWritable(existingPath)
      new File(existingPath) must beWritable
    }
    "provide an beAbsolute matcher to check if a file can is absolute" in {
      new File("/tmp") must beAbsolute
    }
    "provide an beAbsolute matcher to check if a file is absolute" in {
      new File("/tmp") must beAbsolute
    }
    "provide an beHiddenPath matcher to check if a file is hidden" in {
      new File(".tmp") must beHidden
    }
    "provide an beFile matcher to check if a file is a file" in {
      new File("c:/tmp.txt") must beFile
    }
    "provide an beDirectory matcher to check if a file is a directory" in {
      new File("c:/tmp/") must beDirectory
    }
    "provide an haveName matcher to check if a file has a given name" in {
      new File("c:/tmp/test.txt") must haveName("test.txt")
    }
    skip("should be fixed later")
    "provide an haveAbsolutePath matcher to check if a file has a given absolute path" in {
      new File("c:/tmp/test.txt") must haveAbsolutePath("c:/tmp/test.txt")
    }
    "provide an haveCanonicalPath matcher to check if a file has a given canonical path" in {
      new File("c:/tmp/dir/../test.txt") must haveCanonicalPath("c:/tmp/test.txt")
    }
    "provide an haveParent matcher to check if a file has a given parent path" in {
      new File("c:/tmp/dir/test.txt") must haveParent("c:/tmp")
    }
    "provide an haveList matcher to check if a file has a given children" in {
      new File("c:/tmp") must haveList(List("c:/tmp/test.txt"))
    }
  }
}
