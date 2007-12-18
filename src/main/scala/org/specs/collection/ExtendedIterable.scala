package org.specs.collection
import org.specs.collection.ExtendedList._

/**
 * The ExtendedIterable object offers methods applicable to iterables
 */
object ExtendedIterable {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def iterableToExtended[A](xs : Iterable[A]) = new ExtendedIterable(xs)

  /**
   * This class adds various utility methods to iterables
   */
  class ExtendedIterable[A](xs:Iterable[A]) {
    /**
     * @returns a Stream created from the iterable
     */
    def toStream = Stream.fromIterator(xs.elements)

    /**
     * alias for any type of Iterable
     */
    type anyIterable = Iterable[T] forSome {type T} 
    
    /**
     * @returns the representation of the elements of the iterable using the method toString recursively
     */
    def toDeepString: String = {
      "[" + xs.toList.map { x =>
        if (x.isInstanceOf[anyIterable]) x.asInstanceOf[anyIterable].toDeepString else x.toString
      }.mkString(", ") + "]" 
    }
    
    /**
     * @returns true if the 2 iterables contain the same elements according to a function f 
     */
    def isSimilar[B >: A](that: Iterable[B], f: Function2[A, B, Boolean]): Boolean = {
      val ita = xs.elements
      val itb = that.elements
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    
    /**
     * @returns true if the 2 iterables contain the same elements recursively, in the same order 
     */
    def sameElementsAs(that: Iterable[A]): Boolean = {
      val ita = xs.elements.toList
      val itb = that.elements.toList
      var res = true
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: anyIterable, b: anyIterable) => {
          if (a.headOption.isDefined && b.headOption.isDefined) {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            ((x == y) && (resta sameElementsAs restb)) || 
            ((resta.exists(_==y)  && restb.exists(_==x)) && (resta.removeFirst(_==y) sameElementsAs restb.removeFirst(_==x)))
          }
          else
            false
        }
        case _ => ita == itb  
      } 
    }

    /**
     * adds the sameElementsAs method to any object in order to do that comparison recursively 
     */
    implicit def anyToSameElements(x: Any) = new AnyWithSameElements(x)
    class AnyWithSameElements(x: Any) { def sameElementsAs(that: Any): Boolean = {x == that}}
  }
}
