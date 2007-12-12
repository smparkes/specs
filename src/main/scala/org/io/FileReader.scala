package org.specs.io;
import java.io._

trait FileReader {
  def readFile(name:String): String = {
    val in = new BufferedReader(new java.io.FileReader(name));
    val result = new StringBuffer
    appendLines(result, in, in.readLine)
    in.close();
    result.toString
  }
  def appendLines(result: StringBuffer, in: BufferedReader, line: String): Unit = {
    if (line != null) {
      result.append(line)
      result.append("\n")
      appendLines(result, in, in.readLine)
    }
  }
}
