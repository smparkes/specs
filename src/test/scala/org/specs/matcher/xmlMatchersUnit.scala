package org.specs.matcher
import org.specs.runner._
import org.specs.Sugar._
import org.specs.xml.NodeFunctions._
import org.specs.collection.ExtendedIterable._
import scala.xml._
object xmlMatchersUnit extends MatchersSpecification with XmlMatchers {
  "A equals ignore spaces matcher" should { 
    "not take care of spaces when comparing nodes [Alias ==/]" in {
      <a><b/></a> must equalIgnoreSpace(<a> <b/></a>)  
      <a><b/></a> must ==/(<a> <b/></a>)  
    }
    "fail if 2 nodes are not equal, even ignoring spaces" in {
      assertion(<a><b/></a> must equalIgnoreSpace(<a> <c/></a>)) must failWith("<a><b></b></a> is not equal to <a> <c></c></a>")
      assertion(<a><b/></a> aka "the node" must equalIgnoreSpace(<a> <c/></a>)) must failWith("the node <a><b></b></a> is not equal to <a> <c></c></a>")
    }
    "fail if 2 nodes are a Text and an Atom with different data" in {
      assertion(new Atom("hello").toSeq must ==/(new Text("world").toSeq)) must failWith("hello is not equal to world")
      assertion(new Atom("hello").toSeq aka "the seq" must ==/(new Text("world").toSeq)) must failWith("the seq hello is not equal to world")
    }
    "not fail if nodes are not in the same order" in {
      <a><b/><c/></a> must ==/(<a> <c/><b/></a>)  
    }
    "provide a way to specify that the comparison should be ordered" in {
      <a><b/><c/></a> must ==/(<a> <c/><b/></a>).ordered  
    }
  }
  "A \\ matcher" should {
    "match a node <b/> contained in the node a with the label 'b'" in {
      <a><b/></a> must \("b")
    }
    "match a node <b></b> contained in the node a with the label 'b'" in {
      <a><b></b></a> must \("b")
    }
    "not match a node a with the label 'a'" in {
      assertion(<a></a> must \("a")) must failWith("<a></a> doesn't contain subnode a")
      assertion(<a></a> aka "the node" must \("a")) must failWith("the node <a></a> doesn't contain subnode a")
    }
    "not match a node c nested deep inside a node a with the label 'c'" in {
      assertion(<a><b><c></c></b></a> must \("c")) must failWith("<a><b><c></c></b></a> doesn't contain subnode c")
      assertion(<a><b><c></c></b></a> aka "the node" must \("c")) must failWith("the node <a><b><c></c></b></a> doesn't contain subnode c")
    }
    "match a node b contained inside a node a given its label and its attribute name" in {
      <a><b name="value"></b></a> must \("b", ("name"))
    }
    "match a node b contained inside a node a given its label and its attribute names" in {
      <a><b name="value" name2="value"></b></a> must \("b", ("name2", "name"))
    }
    "not match a node b contained inside a node a given its label and a missing attribute name" in {
      assertion(<a><b name2="value"></b></a> must \("b", "name")) must failWith("<a><b name2=\"value\"></b></a> doesn't contain subnode b with attributes: name")
      assertion(<a><b name2="value"></b></a> aka "the node" must \("b", "name")) must failWith("the node <a><b name2=\"value\"></b></a> doesn't contain subnode b with attributes: name")
    }
    "match a node contained in another given its label and its attributes and values" in {
      <a><b name="value"></b></a> must \("b", Map("name"->"value"))
    }
    "not match a node contained in another given its label and a wrong attribute or value" in {
      assertion(<a><b name="value"></b></a> must \("b", Map("name1"->"value"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name1=\"value\"")
      assertion(<a><b name="value"></b></a> must \("b", Map("name"->"value1"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value1\"")
      assertion(<a><b name="value"></b></a> aka "the node" must \("b", Map("name"->"value1"))) must failWith("the node <a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value1\"")
    }
    "not match a node contained in another given its label and a inexisting attributes" in {
      assertion(<a><b name="value"></b></a> must \("b", Map("name"->"value", "name2"->"value2"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value\" name2=\"value2\"")
    }
    "not match a node contained in another given its label and a missing attributes" in {
      assertion(<a><b name="value" name2="value"></b></a> must \("b", Map("name"->"value"))) must failWith("<a><b name2=\"value\" name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value\"")
    }
    "match a node <b><c></c></b> contained in the node a" in {
      <a><b><c></c></b></a> must \(<b><c></c></b>)
    }
    "not match a node <b><d></d></b> not contained in the node a" in {
      assertion(<a><b><c></c></b></a> must \(<b><d></d></b>)) must failWith("<a><b><c></c></b></a> doesn't contain <b><d></d></b>")
    }
    "not evaluate the expressions twice" in {
      val nodes: Iterable[scala.xml.Node] = <c/>
      \("c") must evalOnce(exp(nodes))
    }
  }
  "A \\\\ matcher" should {
    "match a node with its own label" in {
      <a></a> must \\("a")
    }
    "match a node b contained in a node a, with the label 'b'" in {
      <a><b></b></a> must \\("b")
    }
    "match a node c deeply nested in a node a with the label 'c'" in {
      <a><s><c></c></s></a> must \\("c")
    }
    "not match a node whose label doesn't exist" in {
      assertion(<a><b><c></c></b></a> must \\("d")) must failWith("<a><b><c></c></b></a> doesn't contain node d")
      assertion(<a><b><c></c></b></a> aka "the node" must \\("d")) must failWith("the node <a><b><c></c></b></a> doesn't contain node d")
    }
    "match a node given its label even if it has an attribute" in {
      <a><b name="value"></b></a> must \\("b")
    } 
    "match a node given its label and one attribute name" in {
      <a><b name="value"></b></a> must \\("b", "name")
    } 
    "match a node given its label and all of its attribute names" in {
      <a><b><c name="value" name2="value"></c></b></a> must \\("c", ("name2", "name"))
    } 
    "not match a node given its label and one missing attribute name" in {
      assertion(<a><b name2="value"></b></a> must \\("b", "name")) must failWith("<a><b name2=\"value\"></b></a> doesn't contain node b with attributes: name")
    } 
    "match a node given its label and some only of all of its attribute names" in {
      <a><b name="value" name2="value"></b></a> must \\("b", "name")
    } 
    "match a node given its label and all of its attribute names and values" in {
      <a><b><c name="value" name2="value2">string</c></b></a> must \\(<c name2="value2" name="value">string</c>)
    } 
    "match a node given its label, all of its attribute names and values and subnodes" in {
      <a><b att="val"><c name="value" name2="value2">string</c></b></a> must \\(<b att="val"><c name2="value2" name="value">string</c></b>)
    } 
    "match a node <c><d></d></c> deeply contained in the node a" in {
      <a><b><c><d></d></c></b></a> must \\(<c><d></d></c>)
    }
    "not match a node <c><e></e></c> not deeply contained in the node a" in {
      assertion(<a><b><c><d></d></c></b></a> must \\(<c><e></e></c>)) must failWith("<a><b><c><d></d></c></b></a> doesn't contain <c><e></e></c>")
    }
    "not match two nodes if they don't contain the same text" in {
      assertion(<a><b>hello</b></a> must \\(<b>world</b>)) must failWith("<a><b>hello</b></a> doesn't contain <b>world</b>")
    }
    "not match two nodes if they don't contain the same text even if one is an Atom and the other one a Text" in {
      val h = "hello"
      val expected = <a><b>{h}</b></a>  
      assertion(expected must \\(<b>world</b>)) must failWith("<a><b>hello</b></a> doesn't contain <b>world</b>")
    }
    "not evaluate the expressions twice" in {
      val nodes: Iterable[scala.xml.Node] = <c/>
      \\("c") must evalOnce(exp(nodes))
    }
  }
  "\\ and \\\\ matchers" can {
    "be chained with \\ to provide full path searches" in {
      <a><b><c><d></d></c></b></a> must \("b").\("c")
    }
    "be chained with \\\\ to provide more full path searches" in {
      <a><b><c><d></d></c></b></a> must \\("c").\("d")
      <a><b><c><d></d></c></b></a> must \\("b").\\("d")
    }
  }
  "A path matcher" can {
    "be used with the label of the node only (\\)" in {
      <a><s></s></a> must \(<s/>)
    }
    "be used with the label of the node only (\\\\)" in {
      <a><s></s></a> must \\(<s/>)
    }
  }
}
class xmlMatchersTest extends JUnit4(xmlMatchersUnit)