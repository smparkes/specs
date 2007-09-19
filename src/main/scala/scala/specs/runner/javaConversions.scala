package scala.specs.runner

object javaConversions {
  implicit def enumerationToList(e: java.util.Enumeration): List[Object] = {
    var list = List[Object]()
    while (e.hasMoreElements()) { list = e.nextElement::list}
    list
  }
}
