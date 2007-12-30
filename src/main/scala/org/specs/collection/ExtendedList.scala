package org.specs.collection
import scala.collection.immutable._

/**
 * This object provides useful functions for Lists
 */
object ExtendedList {
  /**
   * @returns a list of lists where x is inserted between every element of the original list
   */
  def mix[T](x: T, l: List[T]): List[List[T]] = {
    l match {
      case Nil => List(List(x))
      case y::Nil => List(x::y::Nil, y::x::Nil)
      case y::rest => List(x::l):::mix(x, rest).map((s: List[T]) => y::s)
    }
  }
  /**
   * @returns a list of lists containing permutations of the initial list
   */
  def everyOrder[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x::Nil => List(l)
      case x::rest => everyOrder(rest).flatMap {mix(x, _)}
    }
  }
  /**
   * @returns a list of lists containing permutations of the initial list
   */
  class ExtendedList[T](l: List[T]) {
    def removeFirst(f: T => Boolean): List[T] = {
      l match {
        case Nil => Nil
        case x::rest if (f(x)) => rest
        case x::rest if (!f(x)) => List(x):::rest.removeFirst(f)
      }
    }
    def removeFirstSeq(sublist: List[T]): List[T] = {
      l match {
        case Nil => Nil
        case list if (list.slice(0, sublist.size) == sublist) => list.slice(sublist.size, list.size).toList
        case x::rest => x::rest.removeFirstSeq(sublist)
      }
    }
    def prefixes: List[List[T]] = {
      l match {
        case Nil => Nil
        case x::Nil => List(l)
        case x::rest => List(x)::rest.prefixes.map(x::_)
      }
    }
    def toMap[D](defaultValue: D): scala.collection.immutable.Map[T, D] = {
      var newMap: scala.collection.immutable.Map[T, D] = new HashMap[T, D]
      l.foreach {t:T => newMap = newMap.update(t, defaultValue)}  
      newMap
    }
    def scramble = l.sort((a, b) => (new java.util.Random).nextInt(1) > 0)
  }
  implicit def listToExtendedList[T <: Any](l: List[T]) = new ExtendedList(l)
}
