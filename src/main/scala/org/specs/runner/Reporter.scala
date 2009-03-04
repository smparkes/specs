package org.specs.runner

import scala.collection.mutable.Queue
import org.specs.log.ConsoleLog
import org.specs.specification._

/**
 * The SpecsHolder trait is used by any class providing access to a sequence of specifications
 */
trait SpecsHolder {
  val specs: Seq[Specification]
}

/**
 * An object using the reporter trait should be able to report the result of the execution of several specifications.
 * Those specifications are usually provided by the object when using the reportSpecs method, but
 * the report functionality can also be accessed by passing other specifications directly to the report(specs) method.
 *
 * Any object using the Reporter trait will also inherit a main method controlling:<ul>
 * <li>the reading of command line arguments</li>
 * <li>the display of stacktraces</li>
 * <li>the acception or rejection of some tags</li>
 * </ul>
 *
 * The accepted arguments are:<ul>
 * <li>-ns or --nostacktrace to avoid displaying stacktraces</li>
 * <li>-acc, --accept followed by a comma separated list of tag names for the tags to accept</li>
 * <li>-rej, --reject followed by a comma separated list of tag names for the tags to reject</li>
 * </ul>
 *
 * When subclassing the Reporter trait, the subclasses should usually override the report(specs) method
 * to provide concrete reporting behavior.<p/>
 *
 * Subclasses must not forget to insert a super.report call at the beginning of their processing
 * to allow the chaining of several reporters as traits:
 * object runner extends Runner(spec) with Html with Xml for example
 */
trait Reporter extends SpecsFilter with ConsoleLog {

  /** this variable controls if stacktraces should be printed. */
  protected var stacktrace = true
  /** this variable controls if ok examples should be printed. */
  protected var failedAndErrorsOnly = false
  /** this variable controls if the statistics should be printed. */
  protected var statistics = true

  /** allow subclasses to remove the stacktrace display. */
  def setNoStacktrace(): this.type = { stacktrace = false; this }
  /** allow subclasses to remove the ok and skipped examples. */
  def setFailedAndErrorsOnly(): this.type = { failedAndErrorsOnly = true; this }
  /** allow subclasses to remove the statistics. */
  def setNoStatistics(): this.type = { statistics = false; this }
  /** reset all options. */
  def resetOptions(): this.type = {
    args = Array()
    stacktrace = true
    failedAndErrorsOnly = false
    statistics = false
    this
  }

  /**
   * optional arguments to be used in the main method and which can be set from the code directly.
   */
  var args: Array[String] = Array()

  /**
   * Main method for the Reporter trait.
   *
   * It first agregates all arguments: passed to the class and passed from the command line.
   * Then it calls the reportSpecs method and exit the System with the appropriate error code,
   * depending on the specification success or not.
   */
  def main(arguments: Array[String]) = {
    if (arguments != null)
      args = args ++ arguments
    if (argsContain("-h", "--help")) {
      println("""
usage java <classpath> package.mySpecification [-h|--help]
                                               [-ns|--nostacktrace]
                                               [-nostats|--nostatistics]
                                               [-xonly | -failedonly]
                                               [[-acc | --accept] tag1,tag2,...] [[-rej | --reject] tag1,tag2,...]
                                               [-sus | --system]
                                               [-ex | --example]

-h, --help           prints this message and doesn't execute the specification
-ns, --nostacktrace  removes the stacktraces from the reporting
-nostats, --nostatistics  removes the statistics from the reporting
-xonly, --failedonly reports only failures and errors
-acc, --accept tags  accept only the specified tags (comma-separated names)
-rej, --reject tags  reject the specified tags (comma-separated names)
-sus, --system  only the systems under specifications matching this regular expression will be executed
-ex, --example  only the examples matching this regular expression will be executed
""".stripMargin)
    } else {
      reportSpecs
      if (filteredSpecs.exists(_.isFailing)) System.exit(1) else System.exit(0)
    }
  }
  /** regexp for filtering systems. */
  override def susFilterPattern = argValue(args, List("-sus", "--system")).getOrElse(".*")

  /** regexp for filtering examples. */
  override def exampleFilterPattern = argValue(args, List("-ex", "--example")).getOrElse(".*")

  /** report the list of specifications held by the object mixing this trait. */
  def reportSpecs: this.type = report(this.filteredSpecs)

  /**
   * report specifications.
   *
   * This method should usually be overriden by subclasses to provide concrete reporting behavior.
   * Subclasses must not forget to insert a super.report call at the beginning of their processing
   * to allow the chaining of several reporters as traits.
   */
  def report(specs: Seq[Specification]): this.type = {
    if (argsContain("-ns", "--nostacktrace")) setNoStacktrace()
    if (argsContain("-nostats", "--nostatistics")) setNoStatistics()
    if (argsContain("-xonly", "--failedonly")) setFailedAndErrorsOnly()
    setTags(specs, args)
    this
  }
  /** @return true if the args contain one of the options, regardless of the case. */
  private def argsContain(options: String*) = args.map(_.toLowerCase).exists(options.contains(_))
  /**
   * set the tags passed by the user on the specification.
   * @param specifications list of specifications
   * @param arguments user-defined arguments containing either -acc, --accept, -rej, --reject
   */
  private def setTags(specifications: Seq[Specification], arguments: Array[String]) = {
    def printWarning = warning("accept/reject tags omitted in: " + arguments.mkString(", "))
    def acceptSpecTags(s: Specification, i: Int) = s.acceptTag(arguments(i + 1).split(","):_*)
    def rejectSpecTags(s: Specification, i: Int) = s.rejectTag(arguments(i + 1).split(","):_*)
    def setAcceptedTags(specifications: Seq[Specification], argumentNames: List[String], f: (Specification, Int) => Specification) = {
      arguments.map(_.toLowerCase).findIndexOf(arg => argumentNames.contains(arg)) match {
        case -1 => ()
        case i if (i < arguments.length - 1) => filteredSpecs.foreach(f(_, i))
        case _ => if (!arguments.isEmpty) printWarning
      }
    }
    setAcceptedTags(specifications, List("-acc", "--accept"), acceptSpecTags(_, _))
    setAcceptedTags(specifications, List("-rej", "--reject"), rejectSpecTags(_, _))
  }

  /**
   * @return the argument value in a list of arguments for a given flag in the argumentNames list.
   * for example: argValue(Array("-ex", ".*ex.*"), List("-ex", "--example")) = Some(".*ex.*")
   */
  private def argValue(arguments: Array[String], argumentNames: List[String]): Option[String] = {
    arguments.map(_.toLowerCase).findIndexOf(arg => argumentNames.contains(arg)) match {
      case -1 => None
      case i if (i < arguments.length - 1) => Some(arguments(i + 1))
      case _ => {
        if (!arguments.isEmpty) warning("missing values for flags: " + argumentNames.mkString(", ") + " in " + arguments.mkString(", "))
        None
      }
    }
  }

  def ::(r: Reporter) = List(r, this)
}
