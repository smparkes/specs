package scala.specs.runner

import scala.collection.mutable.Queue
import scala.util.log.ConsoleLog
import scala.util.Timer
import scala.util.SimpleTimer
import scala.io._
import java.util.Calendar

trait Reporter { 
  def report(specs: Iterable[Sut])
}
trait OutputReporter extends Reporter with Output {
  val reporter = this
  val timer: Timer
  def report(specs: Iterable[Sut]) = {
    for(s <- specs) {
      println(s.description + " " + s.verb)
      printExamples(s.examples, "")
      println("")
    }
    val examplesNb = specs.foldLeft(0)(_+_.examples.size)
    val assertionsNb = specs.flatMap(_.examples).foldLeft(0)(_+_.assertionsNb)
    val failuresNb = specs.flatMap(_.examples).foldLeft(0)(_+_.failures.size)
    val errorsNb = specs.flatMap(_.examples).foldLeft(0)(_+_.errors.size)
    def plural[T](nb: int) = if (nb > 1) "s" else ""
    
    println("Finished in " + timer.stop)
    println(examplesNb + " example" + plural(examplesNb) + ", " +
            assertionsNb + " assertion" + plural(assertionsNb) + ", " +
            failuresNb + " failure" + plural(failuresNb) + ", " +
            errorsNb + " error" + plural(errorsNb))
    println("")
  }
  def printExamples(examples: Iterable[Example], padding: String): Unit = {
		for (example <- examples) {
      printExample(example, padding)
      printExamples(example.subExamples, padding + "  ")
    }
  }
  def printExample(example: Example, padding: String) = {
    println(padding + "- " + example.description)
    example.failures.foreach {f: Throwable => println(padding + "  " + f.getMessage) }
    example.errors.foreach {f: Throwable => println(padding + "  " + f.getMessage) }
  }
}

trait ConsoleReporter extends OutputReporter with ConsoleOutput {
  val timer = new SimpleTimer
}