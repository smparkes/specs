package org.specs.log
import org.specs.io.{ConsoleOutput, Output}

trait Log extends Output {
  val Debug = 0
  val Info = 1
  val Warning = 2
  val Error = 3
  var level = Warning
  def debug(msg: String) = if (level == 0) println(msg)
  def info(msg: String) = if (level <= Info ) println(msg)
  def warning(msg: String) = if (level <= Warning ) println(msg)
  def error(msg: String) = if (level <= Error ) println(msg)
}

trait ConsoleLog extends ConsoleOutput with Log

