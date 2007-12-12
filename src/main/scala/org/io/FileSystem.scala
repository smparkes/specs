package org.specs.io;
import java.io._
import org.collection.JavaCollectionsConversion._
import java.util.regex._
import scala.collection.mutable.Queue
import scala.log.Log

trait FileSystem extends FileReader with FileWriter with Log {
  
  def filePaths(path: String) : List[String] = {
    val result = new Queue[String]
    var pattern = globToPattern(path)
    if (isDir(path))
      pattern += "/*.*"
    collectFiles(result, new File("."), pattern)
    result.toList
  }
  
  def collectFiles(result: Queue[String], file: File, pattern: String): Unit = {
    debug("try to accept " + file.getPath.replace("\\", "/") + " with " + pattern)
    if (file.isFile && file.getPath.replace("\\", "/").matches(pattern)) {
      debug("got a match")
      result += (file.getPath)
    }
    else {
      if (file.listFiles != null) file.listFiles.foreach { collectFiles(result, _, pattern) }
    }
  }
  
  def globToPattern(glob: String) = {
    val star = "<STAR>"
    val authorizedNamePattern = "[^\\/\\?<>\\|\\" + star + ":\"]" + star
    var pattern = glob.replace("\\", "/")
    									.replace(".", "\\.")
    									.replace("**/", "(" + authorizedNamePattern + "/)" + star)
                      .replace("*", authorizedNamePattern)
                      .replace(star, "*")
    if (!pattern.startsWith("\\./"))
      pattern = "\\./" + pattern 
    pattern
  }
  def isDir(path: String) = (new File(path)).isDirectory
  def createDir(path: String) = (new File(path)).mkdirs
  def createFile(path: String) = {
    if (!new File(path).getParentFile.exists) createDir(new File(path).getParent) 
    (new File(path)).createNewFile
  }
  def removeDir(path: String): String = {
    val dir = new File(path)
    if (!dir.isDirectory) return dir.getParent
    if (dir.listFiles == null) 
      dir.delete
    else
      dir.listFiles.foreach { file => 
        if (file.isFile) 
          file.delete
        else 
          removeDir(file.getPath)
      }
    return dir.getParent
  }
}
object fs extends FileSystem with ConsoleOutput
