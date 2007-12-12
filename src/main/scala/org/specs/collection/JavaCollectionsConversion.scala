package org.specs.collection;
import java.util.ArrayList

object JavaCollectionsConversion {
  implicit def asList[T](v:java.util.Vector):List[T]= {
    var list:List[T] = List()
    val it:java.util.Iterator = v.iterator
    while (it.hasNext) { list = it.next.asInstanceOf[T]::list}
    list
  }
  implicit def javaArrayToList[T](array: Array[T]) : List[T] = {
    var result = List[T]()
    var i = 0
    if (array == null) return List[T]()
    while (i < array.length) { result = array(i) :: result; i += 1 }
    result
  }
  /** transformation of a <code>java.util.Enumeration</code> to a <code>List</code> object*/
  implicit def enumerationToList(e: java.util.Enumeration): List[Object] = {
    var list = List[Object]()
    while (e.hasMoreElements()) { list = e.nextElement::list}
    list
  }

}
