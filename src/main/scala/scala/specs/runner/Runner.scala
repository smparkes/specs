package scala.specs.runner

import scala.util.log.ConsoleLog
import scala.util.JavaCollectionsConversion

class ConsoleRunner(specifications: Specification*) extends ConsoleReporter {
  def main(args: Array[String]) = report(specifications.flatMap {_.suts })
}

trait SpecsFileRunner extends ConsoleRunner with SpecsFinder {
  def runSpecs(filePath: String) = { 
    val specNames = specificationNames(filePath)
    specNames.foreach { className =>
      val args = new Array[Object](1)
      args(0) = this
      runMethod(className).foreach(m => m.invoke(null, args))
    }
  }
  private def runMethod(className: String) = {
    getClass.getClassLoader.loadClass(className).getDeclaredMethods.find { m => 
        m.getName.eq("execute") && !m.getParameterTypes.isEmpty 
    }
  } 
}