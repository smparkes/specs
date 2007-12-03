package scala.xml
import scala.collection.ExtendedIterable._
import scala.xml.NodeFunctions._

class ExtendedNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoreSpace(ns, n)
    def isEqualIgnoreSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoreSpace(ns, n)
}
class ExtendedNode(n: Node) {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
}
object ExtendedNode {
  implicit def toExtendedNodeSeq(n: NodeSeq) = new ExtendedNodeSeq(n)
  implicit def toExtendedNode(n: Node) = new ExtendedNode(n)
}

object NodeFunctions {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode(n1: Node): Boolean = {n1.label.equals("#PCDATA") && n1.text.matches("\\s*")}

  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def ==/(node: NodeSeq, n: NodeSeq): Boolean = isEqualIgnoreSpace(node, n)

  def isEqualIgnoreSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    (node, n) match {
      case (null, other) => other == null
      case (other, null) => other == null
      case (n1: Node, n2:Node) => (isSpaceNode(n1) && isSpaceNode(n2)) ||
                                  n1.prefix == n2.prefix && 
                                  n1.attributes == n2.attributes && 
                                  n1.label == n2.label && 
                                  n1.child.filter(!isSpaceNode(_)).hasSameElements(n2.child.filter(!isSpaceNode(_)), isEqualIgnoreSpace _)
      case (n1: NodeSeq, n2: NodeSeq) => n1.filter(!isSpaceNode(_)).hasSameElements(n2.filter(!isSpaceNode(_)), isEqualIgnoreSpace _)
    }
  } 
}