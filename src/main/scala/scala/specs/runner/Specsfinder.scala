package scala.specs.runner

import scala.io.FileSystem
import java.util.regex._
import scala.collection.mutable.Queue

trait SpecsFinder extends FileSystem {

  def specificationNames(path: String, pattern: String) : List[String] = {
    var result = new Queue[String]
    filePaths(path).foreach { collectSpecifications(result, _, pattern) }
    result.toList
  }
  
  def collectSpecifications(result: Queue[String], filePath: String, pattern: String): Unit = {
    if (!filePath.endsWith(".scala")) return    
    val specPattern = "\\s*object\\s*(" + pattern + ")\\s*extends\\s*.*Spec.*\\s*\\{"
    val m = Pattern.compile(specPattern).matcher(readFile(filePath))
    while (m.find)
      result += (packageName(filePath) + "." + m.group(1).trim)
  }
  
  def packageName(path: String) = {
    val pattern = "\\s*package\\s*(.+)\\s*"
    val m = Pattern.compile(pattern).matcher(readFile(path))
    if (!m.find) "" else m.group(1).replace(";", "").trim
  }
}
