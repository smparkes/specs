package scala.specs.matcher
import scala.specs.matcher.Matcher._
import scala.specs.matcher.MatcherUtils._

trait MapMatchers {
  def haveKey[S](k: S) = make[Iterable[(S, Any)]]((map: Iterable[(S, Any)]) => (map.exists{case (key, _) => key == k}, map + " has key " + q(k), map + " hasn't key " + q(k))) 
  def haveValue[S](v: S) = make[Iterable[(Any, S)]]((map: Iterable[(Any, S)]) => (map.exists{case (_, value) => value == v}, map + " has value " + q(v), map + " hasn't value " + q(v))) 
  def havePair[S, T](p: (S, T)) = make[Iterable[(S, T)]]((map: Iterable[(S, T)]) => (map.exists{case (key, value) => key == p._1 && value == p._2}, map + " has pair " + q(p), map + " hasn't pair " + q(p)))
  def notHaveKey[S](k: S) = haveKey(k).not 
  def notHaveValue[S](v: S) = haveValue(v).not 
  def notHavePair[S, T](p: (S, T)) = havePair(p).not 
}
