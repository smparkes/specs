package org.specs.io.mock

import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import org.specs.io.Output
import org.specs.io.FileSystem
import java.io._

trait MockFileSystem extends FileSystem {
  var files = new HashMap[String, String]
  override def readFile(path: String) = files(path)
  override def filePaths(path: String) = files.keySet.toList
  def addFile(path: String, content: String) = { files += path -> content }
  override def createFile(path: String) = {files += (path -> ""); true}
  override def getWriter(path: String) = FileMockWriter(path)
  case class FileMockWriter(path: String) extends MockWriter {
    override def write(m: String) : Unit = { files(path) = files(path) + m }
  }
  def reset = files = new HashMap[String, String]
}

trait MockOutput extends Output {
  val messages : Queue[String] = new Queue[String]
  override def println(m : Any) : Unit = { messages += m.toString }
}

trait MockWriter extends java.io.Writer {
  val messages : Queue[String] = new Queue[String]
  var closed = false
  override def write(m: String) : Unit = { messages += m }
  override def close = {closed = true}
  override def flush = {}
  override def write(a: Array[Char], b: Int, c: Int) = {}
}
