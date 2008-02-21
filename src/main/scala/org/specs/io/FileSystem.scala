package org.specs.io;
import java.io._
import java.util.regex._
import org.specs.collection.JavaConversions
import scala.collection.mutable.Queue
import org.specs.log.Log

/**
 * The fs object offers a simple interface to the file system (see the description of the FileSystem trait)
 */
object fs extends FileSystem with ConsoleOutput

/**
 * The FileSystem trait abstract file system operations to allow easier mocking of file system related operations.
 * <p>
 * It mixes the <code>FileReader</code> and <code>FileWrite</code> traits to provide easy read/write operations to files  
 */
trait FileSystem extends FileReader with FileWriter with Log with JavaConversions {
  
  /**
   * @param path glob expression, for example: <code>./dir/**/*.xml</code>
   * @return the list of paths represented by the "glob" definition <code>path</path>  
   */
  def filePaths(path: String): List[String] = {
    var pattern = globToPattern(path)
    if (isDir(path))
      pattern += "/*.*"
    val result = new Queue[String]
    collectFiles(result, new File("."), pattern)
    result.toList
  }
  
  /**
   * @param result list of collected file paths
   * @param file current file being examined
   * @param pattern regular expression which should be matching the file path
   */
  def collectFiles(result: Queue[String], file: File, pattern: String): Unit = {
    debug("try to accept " + file.getPath.replace("\\", "/") + " with " + pattern)
    if (file.isFile && file.getPath.replace("\\", "/").matches(pattern)) {
      debug("got a match")
      result += (file.getPath)
    }
    else if (file.listFiles != null) file.listFiles.foreach { collectFiles(result, _, pattern) }
  }
  
  /**
   * @return the regular expression equivalent to a glob pattern (see the specs for examples)
   */
  def globToPattern(glob: String): String = {
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
  
  /**
   * @return true if the File represented by this path is a directory
   */
  def isDir(path: String) = (new File(path)).isDirectory

  /**
   * creates a directory for a given path
   */
  def createDir(path: String) = (new File(path)).mkdirs

  /**
   * creates a file for a given path. Create the parent directory if necessary
   */
  def createFile(path: String) = {
    if (!new File(path).getParentFile.exists) createDir(new File(path).getParent) 
    new File(path).createNewFile
  }

  /**
   * deletes the directory and all directory contents at the specified path and return the parent path of that directory
   */
  def removeDir(path: String): String = {
    val dir = new File(path)
    if (dir.isDirectory) { 
      if (dir.listFiles == null) 
        dir.delete
      else
        dir.listFiles.foreach { file => 
          if (file.isFile) 
            file.delete
          else 
            removeDir(file.getPath)
        }
    }
    dir.getParent
  }
}
