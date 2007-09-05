package scala.specs.matcher

import scala.specs.matcher.Matcher._

trait PatternMatchers {
  def beLike(a: => (Any => Boolean)) = { 
    make[Any]((v: Any) => ( try { testCase(v)(a).get } catch { case e: scala.MatchError => false }, v + " matches the given pattern", v + " doesn't match the expected pattern"))
  }
  def beNone[T] = make[Option[T]]((v: Option[T]) => ( try { v match {case None => true; case Some(x) => false}} catch { case e: scala.MatchError => false }, v + " is None", v + " is not None"))
  def beSome[T] = new CaseMatcher[T]((v: Option[T]) => ( try { v match {case Some(x) => true; case None => false}} catch { case e: scala.MatchError => false }, v + " is Some(x)", v + " is not Some(x)"))
  def beSomething = beSome[Any]
  class CaseMatcher[T](m: Option[T] => (Boolean, String, String)) extends Matcher[Option[T]](m) {
    private var whichFunction: Option[T => Boolean] = None
    def which(g: T => Boolean) = {whichFunction = Some(g); this}
    override def apply(a: Option[T]) = whichFunction match {
      case None => m(a)
      case Some(g) => {
          (try { a match {case Some(x) => g(x); case None => false}} catch { case e: scala.MatchError => false }, 
              "there is a Some(x) verifying the given property", "there is no Some(x) verifying the given property") 
        }
    } 
  } 
  def testCase(a: Any)(v: => (Any => Boolean)) = Some(a) map v
}
