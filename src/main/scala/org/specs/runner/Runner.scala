package org.specs.runner
import org.specs.log.ConsoleLog
import org.specs.collection.JavaCollectionsConversion

/**
 * This class implements the <code>ConsoleReporter</code> by adding a main 
 * method.<br>
 * Usage: <code>object mySpecRunner extends ConsoleRunner(mySpec1, mySpec2)</code>
 */  
class ConsoleRunner(specifications: Specification*) extends ConsoleReporter {
   def ConsoleRunner(specs: List[Specification]) = new ConsoleRunner(specs :_*)
   def main(args: Array[String]) = report(specifications)
}

/**
 * This class can be used to search for specifications on a given path 
 * and execute them.<br>
 * Usage: <code>object myFileRunner extends SpecsFileRunner(path, pattern)</code><br>
 * Where <code>path</code> is a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
 * and <code>pattern</code> is a regular expression which is supposed to match an object name extending a Specification
 * class named ".*Spec.*"
 */  
class SpecsFileRunner(path: String, pattern: String) extends ConsoleRunner with SpecsFinder {
  
  /** 
   * overrides the <code>report</code> method in <code>ConsoleRunner</code>
   * which is called by default in the main method<br>
   * In that case, the <code>specifications</code> parameter is an empty list
   * but the list of specifications to report is build from specification names found on the path
   * indicated by the <code>path</code> parameter
   */
  override def report(specifications: Iterable[Specification]) = {
    var specList: List[Specification] = Nil
    specificationNames(path, pattern) foreach {className => 
      createSpecification(className) match {
        case Some(s) => specList = s::specList
        case None => {println("Could not load " + className);()}
      }
    }
    // this specification is added for better reporting
    object totalSpecification extends Specification {
      new java.io.File(path).getAbsolutePath isSpecifiedBy(specList: _*)
    }
    super.report(List(totalSpecification))
  } 
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   * @return None in case of an exception. 
   */
  def createSpecification(className: String): Option[Specification] = {
    try {
     return Some(getClass.getClassLoader.loadClass(className).newInstance.asInstanceOf[Specification])
    } catch {
      case e => ()
    }
    return None
  }
}