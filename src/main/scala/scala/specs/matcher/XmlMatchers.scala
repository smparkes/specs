package scala.specs.matcher
import scala.xml._

trait XmlMatchers {
  def \\(node: => Node) = new Matcher[Iterable[Node]]() {
    def apply(nodes: =>Iterable[Node]) = (nodes.exists(!_.\\(node.label).isEmpty), nodes + " contains " + node, nodes + " doesn't contain " + node)
  } 
  def \(node: => Node) = new Matcher[Iterable[Node]]() {
    def apply(nodes: =>Iterable[Node]) = (nodes.exists(!_.\(node.label).isEmpty), nodes + " contains " + node, nodes + " doesn't contain " + node)
  } 
}
