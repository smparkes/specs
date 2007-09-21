package scala.specs.matcher
import scala.specs.matcher.MatcherUtils._

/**
 * The <code>MapMatchers</code> trait provides matchers which are applicable to Map objects
 * It currently accepts any Iterable[(K, V)] whereas it should only accept Map[K, V]
 * This is because the implicit defs in the {@link <code>SpecificationStructure</code>} trait
 * are picking up iterables in general
 */
trait MapMatchers {

  /**
   * Matches if map.contains(k)
   */   
  def haveKey[S](k: S) = new Matcher[Iterable[(S, Any)]](){ 
     def apply(map: => Iterable[(S, Any)]) = (map.exists{ p => p._1 == k}, map + " has key " + q(k), map + " hasn't key " + q(k))
  } 

  /**
   * Matches if not(map.contains(k))
   */   
  def notHaveKey[S](k: S) = haveKey(k).not 

  /**
   * Matches if map contains a pair (key, value) with value == v
   */   
  def haveValue[S](v: S) = new Matcher[Iterable[(Any, S)]](){ 
     def apply(map: => Iterable[(Any, S)]) = (map.exists(p => p._2 == v), map + " has value " + q(v), map.toString + " hasn't value " + q(v)) 
   }

  /**
   * Matches if map doesn't contain a pair (key, value) with value == v
   */   
  def notHaveValue[S](v: S) = haveValue(v).not 

  /**
   * Matches if map contains a pair (key, value) == (k, v)
   */   
  def havePair[S, T](p: (S, T)) = new Matcher[Iterable[(S, T)]](){
     def apply(map: => Iterable[(S, T)]) = (map.exists{case e => e == p}, map + " has pair " + q(p), map + " hasn't pair " + q(p))
   }

  /**
   * Matches if map doesn't contain a pair (key, value) == (k, v)
   */   
  def notHavePair[S, T](p: (S, T)) = havePair(p).not 
}
