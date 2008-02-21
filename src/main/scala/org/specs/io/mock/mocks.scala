package org.specs.io.mock

import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import org.specs.io.Output
import org.specs.io.FileSystem
import java.io._

/**
 * The MockFileSystem trait mocks the FileSystem by storing a Map[path, content] representing the content of the FileSystem
 */
trait MockFileSystem extends FileSystem {

  /** default extension which can be used when creating default file names */
  var defaultExtension = ""

  /** this map associates some file paths with file contents */
  var files = new HashMap[String, String]

  /** @return the content of a file corresponding to a given path */
  override def readFile(path: String) = files(path)

  /** @return all file paths */
  override def filePaths(path: String) = files.keySet.toList

  /** adds a new file to the FileSystem. The file path will be a default one */
  def addFile(content: String) = files += defaultFilePath -> content

  /** adds a new file to the FileSystem with a specific file path */
  def addFile(path: String, content: String) = files += path -> content

  /** @return a default file path. All default file paths will be different from each other */
  def defaultFilePath = "name" + files.size + defaultExtension

  /** creates a file with the specified path but an empty content */
  override def createFile(path: String) = {files += (path -> ""); true}

  /** @returns a mock FileWriter for a specific path */
  override def getWriter(path: String) = MockFileWriter(path)

  case class MockFileWriter(path: String) extends MockWriter {
    override def write(m: String): Unit = files(path) = files(path) + m
  }
  /** removes all specified files */
  def reset = files = new HashMap[String, String]
}

/**
 * The MockOutput trait catches all messages printed with the Output trait
 */
trait MockOutput extends Output {

  /** list of messages representing the output */
  val messages : Queue[String] = new Queue[String]

  /** adds m.toString to a list of messages */
  override def println(m : Any) : Unit = messages += m.toString
}

/**
 * The MockWriter writes all the content written with the Writer interface to a Queue of Strings
 */
trait MockWriter extends java.io.Writer {

  /** list of messages representing the output */
  val messages : Queue[String] = new Queue[String]

  /** is the Writer closed? */
  var closed = false
  override def write(m: String) : Unit = messages += m

  /** closes the Writer */
  override def close = closed = true

  /** flushes the Writer */
  override def flush = {}

  /** overrides the write(a: Array[Char], b: Int, c: Int) method to do nothing */
  override def write(a: Array[Char], b: Int, c: Int) = {}
}
