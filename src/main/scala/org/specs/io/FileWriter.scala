package org.specs.io;
import java.io._

trait FileWriter {
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
  def getWriter(path: String): Writer = new BufferedWriter(new java.io.FileWriter(path))
}
