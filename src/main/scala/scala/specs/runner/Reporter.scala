package scala.specs.runner

import scala.collection.mutable.Queue
import scala.util.log.ConsoleLog
import scala.util.Timer
import scala.util.SimpleTimer
import scala.io._
import java.util.Calendar

trait Reporter { 
  def report(specs: Iterable[Specification]): Unit
  def report(specs: Specification*): Unit = report(specs: _*)
}
trait OutputReporter extends Reporter with Output {
  val reporter = this
  val timer: Timer
  def report(specs: Iterable[Specification]): Unit = specs foreach (reportSpec(_, ""))
  def report(specs: Iterable[Specification], padding: String): Unit = specs foreach (reportSpec(_, padding))
  def reportSpec(spec: Specification): Unit = reportSpec(spec, "")
  def reportSpec(spec: Specification, padding: String): Unit = {
    println(padding + "Specification \"" + spec.name + "\"")
    report(spec.subSpecifications, padding + "  ")
    reportSuts(spec.suts, padding + "  ")
    println(padding + "Total for specification \"" + spec.name + "\":")
    printStats(stats(spec), padding)   
  }
  
  def stats(spec: Specification): (Int, Int, Int, Int) = {
    val examplesNb = spec.suts.foldLeft(0)(_+_.examples.size) + spec.subSpecifications.foldLeft(0)(_ + stats(_)._1) 
    val assertionsNb = spec.suts.flatMap(_.examples).foldLeft(0)(_+_.assertionsNb) + spec.subSpecifications.foldLeft(0)(_ + stats(_)._2)
    val failuresNb = spec.suts.flatMap(_.examples).foldLeft(0)(_+_.failures.size) + spec.subSpecifications.foldLeft(0)(_ + stats(_)._3)
    val errorsNb = spec.suts.flatMap(_.examples).foldLeft(0)(_+_.errors.size) + spec.subSpecifications.foldLeft(0)(_ + stats(_)._4)
    (examplesNb, assertionsNb, failuresNb, errorsNb)
  }
  
  def reportSuts(suts: Iterable[Sut], padding: String) = {
    if (suts.toList.size > 1) 
      suts foreach {reportSut(_, padding)}
    else
      suts foreach {printSut(_, padding)}
  }
  def reportSut(sut: Sut, padding: String) = { printSut(sut, padding); printStats(sut, padding) }
  def printSut(sut: Sut, padding: String) = {
    println(padding + sut.description + " " + sut.verb)
    reportExamples(sut.examples, padding)
    println("")
  }
  def printStats(sut: Sut, padding: String): Unit = {
    val examplesNb = sut.examples.size
    val assertionsNb = sut.examples.foldLeft(0)(_+_.assertionsNb)
    val failuresNb = sut.examples.foldLeft(0)(_+_.failures.size)
    val errorsNb = sut.examples.foldLeft(0)(_+_.errors.size)
    println(padding + "Total for SUT \"" + sut.description + "\":")
    printStats((examplesNb, assertionsNb, failuresNb, errorsNb), padding)    
  }
  
  def printStats(stat: (Int, Int, Int, Int), padding: String) = {
    val (examplesNb, assertionsNb,  failuresNb, errorsNb) = stat
    def plural[T](nb: int) = if (nb > 1) "s" else ""
    println(padding + "Finished in " + timer.stop)
    println(padding + examplesNb + " example" + plural(examplesNb) + ", " +
            assertionsNb + " assertion" + plural(assertionsNb) + ", " +
            failuresNb + " failure" + plural(failuresNb) + ", " +
            errorsNb + " error" + plural(errorsNb))
    println("")
  }
  def reportExamples(examples: Iterable[Example], padding: String): Unit = {
		for (example <- examples) {
      reportExample(example, padding)
      reportExamples(example.subExamples, padding + "  ")
    }
  }
  def reportExample(example: Example, padding: String) = {
    def status(example: Example) = if (example.errors.size + example.failures.size > 0) "x " else "+ "
    println(padding + status(example) + example.description)
    example.failures.foreach {f: Throwable => println(padding + "  " + f.getMessage) }
    example.errors.foreach {f: Throwable => println(padding + "  " + f.getMessage) }
  }
}

trait ConsoleReporter extends OutputReporter with ConsoleOutput {
  val timer = new SimpleTimer
}