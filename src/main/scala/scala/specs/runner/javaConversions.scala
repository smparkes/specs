package scala.specs.runner

object javaConversions {
  /** transformation of a <code>java.util.Enumeration</code> to a <code>List</code> object*/
  implicit def enumerationToList(e: java.util.Enumeration): List[Object] = {
    var list = List[Object]()
    while (e.hasMoreElements()) { list = e.nextElement::list}
    list
  }
}
