package org.specs.collection
import scala.collection.immutable._

/**
 * This object provides useful functions for Lists, like:<ul>
 * <li><code>everyOrder</code>: returns all permutations of the list elements 
 * <li><code>mix</code>: returns all the lists made by adding one element at any place of an existing list
 * <li><code>removeFirst</code>: returns a list minus the first elements matching a given element
 * </ul>
 */
object ExtendedList {
  /**
   * @return a list of lists where x is inserted between every element of the original list
   */
  def mix[T](x: T, l: List[T]): List[List[T]] = {
    l match {
      case Nil => List(List(x))
      case y::Nil => List(x::y::Nil, y::x::Nil)
      case y::rest => List(x::l):::mix(x, rest).map((s: List[T]) => y::s)
    }
  }
  /**
   * @return a list of lists containing permutations of the initial list
   */
  def everyOrder[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x::Nil => List(l)
      case x::rest => everyOrder(rest).flatMap {mix(x, _)}
    }
  }
  /**
   * See the description of the ExtendedList object
   */
  class ExtendedList[T](l: List[T]) {
    /**
     * @return a list minus the first element satisfying the predicateof lists containing permutations of the initial list
     */
    def removeFirst(predicate: T => Boolean): List[T] = {
      l match {
        case Nil => Nil
        case x::rest if (predicate(x)) => rest
        case x::rest if (!predicate(x)) => List(x):::rest.removeFirst(predicate)
        case _ => Nil // should never happen thanks to the predicate condition above
      }
    }
    /**
     * @return a list minus the first occurence of the sublist
     */
    def removeFirstSeq(sublist: List[T]): List[T] = {
      l match {
        case Nil => Nil
        case list if (list.slice(0, sublist.size) == sublist) => list.slice(sublist.size, list.size).toList
        case x::rest => x::rest.removeFirstSeq(sublist)
      }
    }

    /**
     * @return all the prefixes of a list: [1, 2, 3].prefixes == [[1], [1, 2], [1, 2, 3]]
     */
    def prefixes: List[List[T]] = {
      l match {
        case Nil => Nil
        case x::Nil => List(l)
        case x::rest => List(x)::(rest).prefixes.map(x::_)
      }
    }

    /**
     * @return a map where all the keys are the list element and having a default value for all keys
     */
    def toMap[D](defaultValue: D): scala.collection.immutable.Map[T, D] = {
      var newMap: scala.collection.immutable.Map[T, D] = new HashMap[T, D]
      l.foreach {t:T => newMap = newMap.update(t, defaultValue)}  
      newMap
    }

    /**
     * @return a randomly mixed list
     */
    def scramble = l.sort((a, b) => (new java.util.Random).nextInt(1) > 0)
  }
  implicit def listToExtendedList[T](l: List[T]) = new ExtendedList(l)
}
