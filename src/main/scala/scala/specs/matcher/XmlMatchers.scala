package scala.specs.matcher
import scala.xml._

trait XmlMatchers {
  def \\(label: String): XmlMatcher = new XmlMatcher(List(new ElementsSearch(label)))
  def \\(label: String, attributes: List[String]): XmlMatcher = new XmlMatcher(List(new ElementsSearch(label, attributes)))
  def \\(label: String, attributeValues: Map[String, String]): XmlMatcher = new XmlMatcher(List(new ElementSearch(label, attributeValues)))
  def \\(node: Node): XmlMatcher = \\(node.label)
  def \(label: String): XmlMatcher = new XmlMatcher(List(new ElementSearch(label)))
  def \(label: String, attributes: List[String]): XmlMatcher = new XmlMatcher(List(new ElementSearch(label, attributes)))
  def \(label: String, attributeValues: Map[String, String]): XmlMatcher = new XmlMatcher(List(new ElementSearch(label, attributeValues)))
  def \(node: Node): XmlMatcher = \(node.label)
}
case class XmlMatcher(functions: List[PathFunction]) extends Matcher[Iterable[Node]]() {
  def apply(nodes: =>Iterable[Node]) = checkFunctions(functions, nodes)
  def checkFunctions(pathFunctions: List[PathFunction], nodes: Iterable[Node]): (Boolean, String, String) = {
    def searchedElements(function: PathFunction) = {
      function.nodeLabel + (if (function.attributes.isEmpty && function.attributeValues.isEmpty) 
                              "" 
                            else 
                              " with attributes: " + function.searchedAttributes)
    }
    pathFunctions match {
      case Nil => (true, "no node to search", "no node to search")       
      case function::Nil => {
        (!function(nodes).isEmpty, nodes + " contains " + searchedElements(function), nodes + " doesn't contain " + searchedElements(function))
      }       
      case function::rest => {
        val matchedNodes = function(nodes) 
        if (matchedNodes.isEmpty)
         (false, nodes + " contains " + searchedElements(function), nodes + " doesn't contain " + searchedElements(function))
        else
         checkFunctions(rest, matchedNodes)
      }       
    }
  }
  def \(label: String): XmlMatcher = new XmlMatcher(functions:::List(new ElementSearch(label, Nil)))
  def \\(label: String): XmlMatcher = new XmlMatcher(functions:::List(new ElementsSearch(label, Nil)))
  def \(node: Node): XmlMatcher = \(node.label)
  def \\(node: Node): XmlMatcher = \\(node.label)
  def applyFunctions(nodes: Iterable[Node]) = {true}
}  

sealed abstract class PathFunction(val label: String, val attributes: List[String], val attributeValues: Map[String, String]) extends Function1[Iterable[Node], Iterable[Node]] {
  def matchNode(node: Node, attributes: List[String], attributeValues: Map[String, String]) = {
    def metaDataMatch(m: MetaData, attributes: List[String], attributeValues: Map[String, String]) = {
      if (!attributes.isEmpty)
        m.map((a: MetaData) => a.key).toList.intersect(attributes) == attributes
      else
        Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*) == attributeValues
    }
    node match {
      case Node(_, metadatas) if (metaDataMatch(metadatas, attributes, attributeValues)) => true
      case Node(_, metadatas, _) if (metaDataMatch(metadatas, attributes, attributeValues)) => true
      case _ => false
    }
  }
  def nodeLabel: String
  def searchedAttributes = {
    if (!attributes.isEmpty) 
      attributes.mkString(", ") 
    else
      attributeValues.map(a=> a._1 + "=\"" + a._2 + "\"").mkString(" ")
  }
}
class ElementSearch(override val label: String, override val attributes: List[String], override val attributeValues: Map[String, String]) extends PathFunction(label, attributes, attributeValues) {
  def this(label: String) = this(label, Nil, Map.empty)
  def this(label: String, attributes: List[String]) = this(label, attributes, Map.empty)
  def this(label: String, attributeValues: Map[String, String]) = this(label, Nil, attributeValues)
  def apply(nodes: Iterable[Node]): Iterable[Node] = for(node <- nodes;
                                                         found <- node.\(label) if (matchNode(found, attributes, attributeValues))) yield found 
  def nodeLabel: String = ("subnode " + label)
}
class ElementsSearch(override val label: String, override val attributes: List[String], override val attributeValues: Map[String, String]) extends PathFunction(label, attributes, attributeValues) {
  def this(label: String) = this(label, Nil, Map.empty)
  def this(label: String, attributes: List[String]) = this(label, attributes, Map.empty)
  def this(label: String, attributeValues: Map[String, String]) = this(label, Nil, attributeValues)
  def apply(nodes: Iterable[Node]): Iterable[Node] = for(node <- nodes;
                                                         found <- node.\\(label) if (matchNode(found, attributes, attributeValues))) yield found
  def nodeLabel: String = ("node " + label)
}
