package scala.specs.runner
import scala.util.log.ConsoleLog
import scala.util.JavaCollectionsConversion

/**
 * This class implements the <code>ConsoleReporter</code> by adding a main 
 * method.
 * Usage: <code>object mySpecRunner extends ConsoleRunner(mySpec1, mySpec2)</code>
 */  
class ConsoleRunner(specifications: Specification*) extends ConsoleReporter {
   def ConsoleRunner(specs: List[Specification]) = new ConsoleRunner(specs :_*)
   def main(args: Array[String]) = report(specifications)
}

/**
 * This class can be used to search for specifications on a given path 
 * and execute them
 * Usage: <code>object myFileRunner extends SpecsFileRunner(filePath); myFileRunner.runSpecs</code>
 */  
class SpecsFileRunner(filePath: String, pattern: String) extends ConsoleRunner with SpecsFinder {
  override def report(specifications: Iterable[Specification]) = {
    var specList: List[Specification] = Nil
    specificationNames(filePath, pattern) foreach {className => 
      createSpecification(className) match {
        case Some(s) => specList = s::specList
        case None => ()
      }
    }
    object totalSpecification extends Specification {
      "Specifications found at " + new java.io.File(filePath).getAbsolutePath isSpecifiedBy(specList: _*)
    }
    super.report(List(totalSpecification))
  } 
  def createSpecification(className: String): Option[Specification] = {
    try {
     return Some(getClass.getClassLoader.loadClass(className +"$").newInstance.asInstanceOf[Specification])
    } catch {
      case e => ()
    }
    return None
  }
}