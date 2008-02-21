package org.specs.io;
import java.io._

/**
 * The FileWriter trait provides a simple function to write a String to a file
 */
trait FileWriter {

  /**
   * writes some content to a file and take care of closing the file.<p>
   * Usage: <code>
   * write("./dir/hello.txt") { out =>
   *   out.write("content")
   * }
   * </code>
   * @param path path of the file to read
   */
  def write(path: String)(function: Writer => Unit): Unit = {
    val out = getWriter(path)
    try {
      function(out)
    } finally {
      try {
      out.close()
      } catch { case _ => }
    }
  }
  /**
   * The getWriter function can be overriden to provide a mock writer writing in the console for example
   * @return a Writer object opened on the file designated by <code>path</code>
   */
  def getWriter(path: String): Writer = new BufferedWriter(new java.io.FileWriter(path))
}
