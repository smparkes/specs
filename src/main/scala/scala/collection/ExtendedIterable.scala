package scala.collection

import scala.collection.ExtendedList._

object ExtendedIterable {
  implicit def iterableToExtended[A](xs : Iterable[A]) = new ExtendedIterable(xs)
  class ExtendedIterable[A](xs:Iterable[A]){
    def toStream = Stream.fromIterator(xs.elements)
    type anyIterable = Iterable[T] forSome {type T} 
    
    def toDeepString: String = {
      "[" + xs.toList.map { x =>
        if (x.isInstanceOf[anyIterable]) x.asInstanceOf[anyIterable].toDeepString else x.toString
      }.mkString(", ") + "]" 
    }
    def sameElement[B >: A](that: Iterable[B], f:Function2[A, B, Boolean]): Boolean = {
      val ita = xs.elements
      val itb = that.elements
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    
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
        case (a, b) => a == b  
      } 
    }
    implicit def anyToSameElements(x: Any) = new Object { def sameElementsAs(that: Any): Boolean = {x == that}}
  }
}
