package scala.specs

import scala.specs._
import scala.specs.matcher._
import scala.specs.Sugar._
import scala.specs.integration._
import scala.util._
import scala.collection.mutable._
import scalacheck.Gen._
import scala.specs.matcher.MatcherUtils._

object specificationUnitSuite extends JUnit3(specificationUnit)
object specificationUnit extends Specification {

  "A specification" should {
    "have a description corresponding to its unqualified class name, whatever the class name" in {
      def classNames = for {
        packageName <- elements("com", "scala")
        className <- elements(packageName + "s", packageName + ".specs", packageName + ".other.normal")
        name <- elements(className, className + "$inner", className + "$inner$", className + "$2", className + "$2$")
      } yield name

      classNames must pass { name : String => 
        specification.createDescription(name) must (not(beMatching("\\$")) and 
                                           not(beMatching("\\.")) and
                                           not(beInt))
      }
   }
  }
  def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def beInt = Matcher.make[String](s => (isInt(s), q(s) + " is an integer", q(s) + " is not an integer"))
 
  object specification extends Specification
}



