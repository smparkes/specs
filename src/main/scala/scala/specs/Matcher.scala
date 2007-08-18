package scala.specs
import java.util.regex._
import scala.specs.integration.sCheck._
import scala.specs.integration.ScalaCheck
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._

trait Matchers extends ScalaCheck {
  def not[T](matcher: Matcher[T]) = matcher.not
  def beIn[T <: AnyRef](iterable: Iterable[T]) = Matcher[T]((a: T) => (iterable.exists(_ == a), q(a) + " is in " + iterable, q(a) + " is not in " + iterable)) 
  def contain[T <: AnyRef](a: T) = Matcher[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists(_ == a), iterable + " contains " + q(a), iterable + " doesn't contain " + q(a))) 
  def exist[T <: AnyRef](function: T => Boolean) = Matcher[Iterable[T]]((iterable: Iterable[T]) => (iterable.exists{function(_)}, "at least one element verifies the property in " + iterable, "no element verifies the property in " + iterable)) 
  def existMatch(pattern: String) = Matcher[Iterable[String]]((iterable: Iterable[String]) => (iterable.exists( matches(pattern) _), "at least one element matches " + q(pattern) + " in " + iterable, "no element matches " + q(pattern) + " in " + iterable)) 

  def be[T](a: T) = Matcher[T]((b: T) => (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef], q(b) + " is the same as " + q(a), q(b) + " is not the same as " + q(a))) 
  def is_==[T](a: T) = Matcher[T]((b:T) => ((a == b), q(b) + " is equal to " + q(a), q(b) + " is not equal to " + q(a))) 
  def equalIgnoreCase[T <: String](a: T) = Matcher[T]((b:T) => (a.equalsIgnoreCase(b), q(b) + " is equal ignoring case to " + q(a), q(b) + " is not equal ignoring case to " + q(a))) 
  def include[T <: String](a: String) = Matcher[T]((b: T) => (b.indexOf(a) >= 0, q(b) + " includes " + q(a), q(b) + " doesn't include " + q(a))) 
  def beMatching[T <: String](a: String) = Matcher[T]((b: T) => (matches(a)(b), q(b) + " matches " + q(a), q(b) + " doesn't match " + q(a))) 
  def startWith[T <: String](a: String) = Matcher[T]((b: T) => (b.startsWith(a), q(b) + " starts with " + q(a), q(b) + " doesn't start with " + q(a))) 
  def endWith[T <: String](a: String) = Matcher[T]((b: T) => (b.endsWith(a), q(b) + " ends with " + q(a), q(b) + " doesn't end with " + q(a))) 
  def beEmpty[S <: Any {def isEmpty: Boolean}] = Matcher[S]((iterable: S) => (iterable.isEmpty, iterable + " is empty", iterable + " is not empty")) 
  def haveKey[S](k: S) = Matcher[Iterable[(S, Any)]]((map: Iterable[(S, Any)]) => (map.exists{case (key, _) => key == k}, map + " has key " + q(k), map + " hasn't key " + q(k))) 
  def haveValue[S](v: S) = Matcher[Iterable[(Any, S)]]((map: Iterable[(Any, S)]) => (map.exists{case (_, value) => value == v}, map + " has value " + q(v), map + " hasn't value " + q(v))) 
  def havePair[S, T](p: (S, T)) = Matcher[Iterable[(S, T)]]((map: Iterable[(S, T)]) => (map.exists{case (key, value) => key == p._1 && value == p._2}, map + " has pair " + q(p), map + " hasn't pair " + q(p)))
  def beClose[S <% Double](n: S, delta: S) = Matcher[S]((x: S) => ((n - delta <= x) && (x <= n + delta), x + " is close " + n + " +/- " + delta, x + " is not close " + n + " +/- " + delta))
  def beLike(a: => (Any => Boolean)) = { 
    Matcher[Any]((v: Any) => ( try { testCase(v)(a).get } catch { case e: scala.MatchError => false }, v + " matches the given pattern", v + " doesn't match the expected pattern"))
  }
  def beNone[T] = Matcher[Option[T]]((v: Option[T]) => ( try { v match {case None => true; case Some(x) => false}} catch { case e: scala.MatchError => false }, v + " is None", v + " is not None"))
  def beSome[T] = new CaseMatcher[T]((v: Option[T]) => ( try { v match {case Some(x) => true; case None => false}} catch { case e: scala.MatchError => false }, v + " is Some(x)", v + " is not Some(x)"))
  def beSomething = beSome[Any]
  class CaseMatcher[T](m: Option[T] => (Boolean, String, String)) extends Matcher[Option[T]](m) {
    private var whichFunction: Option[T => Boolean] = None
    def which(g: T => Boolean) = {whichFunction = Some(g); this}
    override def apply(a: Option[T]) = whichFunction match {
      case None => matcher(a)
      case Some(g) => {
          (try { a match {case Some(x) => {println(x); println(g(x));g(x)}; case None => false}} catch { case e: scala.MatchError => false }, 
              "there is a Some(x) verifying the given property", "there is no Some(x) verifying the given property") 
        }
    } 
  } 
  def notContain[T <: AnyRef](a: T) = not(contain(a)) 
  def notExist[T <: AnyRef](function: T => Boolean) = not(exist(function)) 
  def notBeIn[T <: AnyRef](iterable: Iterable[T]) = not(beIn(iterable)) 
  def containMatch(pattern: String) = existMatch(pattern) 
  def notExistMatch(pattern: String) = not(existMatch(pattern))
  def notBe[T](a: T) = not(be(a)) 
  def notEq[T](a: T) = not(be(a)) 
  def is_!=[T](a: T) = not(is_==(a)) 
  def notInclude[T <: String](a: T) = not[T](include[T](a)) 
  def notEqualIgnoreCase[T <: String](a: T) = not(equalIgnoreCase(a)) 
  def notBeMatching(a: String) = not[String](beMatching[String](a)) 
  def notStartWith(a: String) = not[String](startWith[String](a))
  def notEndWith(a: String) = not[String](endWith[String](a))

  def notBeEmpty[S <: Any {def isEmpty: Boolean}] = not(beEmpty[S])
  def isEmpty = beEmpty
  def isNotEmpty = notBeEmpty 

  def notHaveKey[S](k: S) = not(haveKey(k)) 
  def notHaveValue[S](v: S) = not(haveValue(v)) 
  def notHavePair[S, T](p: (S, T)) = not(havePair(p)) 
  def function[T](f: T => Boolean) = Matcher((x: T) => (f(x), x + " verifies the property", x + " doesn't verify the expected property"))
  def pass[T](g: Gen[T]) = {
    implicit def toGenerator(c: Arbitrary[T]): Gen[T] = g
    Matcher[Function1[T, Boolean]]( (x: Function1[T, Boolean]) => {
      check(x) match {
        case TestStats(TestPropException(FailureException(msg), ex), _, _) => (false, "", "A counter-example is '"+ex(0).toString+"': " + msg) 
        case _ => (true, "The property passed without any counter-example", "") 
      }
    })
  }
  
  def matches[T <: String](a: String)(b: T) = Pattern.compile(a).matcher(b).find 
  def q(a: Any)  = "'" + a.toString + "'"
  def testCase(a: Any)(v: => (Any => Boolean)) = Some(a) map v
}
case class Matcher[T](matcher: T => (Boolean, String, String)) {
  def apply(a: T) = matcher(a) 
  def and(m: Matcher[T]) : Matcher[T] = Matcher((a: T) => {
      val result1 = this(a)
      if (!result1._1)
        (false, result1._2, result1._3)
      else {
        val result2 = m(a) 
        (result2._1, result1._2 + " and " + result2._2, result1._2 + " but " + result2._3) 
      }
  })
  def or(m: Matcher[T]) : Matcher[T] = Matcher((a: T) => {
    val result1 = this(a)
    val result2 = m(a) 
      if (!result1._1)
        (result2._1, result2._2, result1._3 + " and " + result2._3) 
      else if (!result2._1)
        (result1._1, result1._2, result1._3 + " and " + result2._3)
      else
        (result1._1 || result2._1, result1._2 + " and " + result2._2, result1._3 + " and " + result2._3) 
  })
  def not = Matcher((a: T) => {
    val result = matcher(a)
    (!result._1, result._3, result._2)
  })
}
