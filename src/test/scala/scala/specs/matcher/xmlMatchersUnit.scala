package scala.specs.matcher
import scala.specs.runner._
import junit.framework._
import scala.specs.Sugar._

class MyTest extends TestCase

object xmlMatchersSuite extends JUnit3(xmlMatchersUnit)
object xmlMatchersUnit extends MatchersSpecification with XmlMatchers {
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
    }
    "match a node given its label and one attribute name" in {
      <a><b name="value"></b></a> must \\("b", "name")
    } 
    "match a node given its label and all of its attribute names" in {
      <a><b><c name="value" name2="value"></c></b></a> must \\("c", ("name2", "name"))
      assertion(<a><b name2="value"></b></a> must \\("b", "name")) must failWith("<a><b name2=\"value\"></b></a> doesn't contain node b with attributes: name")
    } 
    "not match a node given its label and one missing attribute name" in {
      assertion(<a><b name2="value"></b></a> must \\("b", "name")) must failWith("<a><b name2=\"value\"></b></a> doesn't contain node b with attributes: name")
    } 
    "match a node given its label and some only of all of its attribute names" in {
      <a><b name="value" name2="value"></b></a> must \\("b", "name")
    } 
  }
  "A \\ matcher" should {
    "match a node b contained in the node a with the label 'b'" in {
      <a><b></b></a> must \("b")
    }
    "not match a node a with the label 'a'" in {
      assertion(<a></a> must \("a")) must failWith("<a></a> doesn't contain subnode a")
    }
    "not match a node c nested deep inside a node a with the label 'c'" in {
      assertion(<a><b><c></c></b></a> must \("c")) must failWith("<a><b><c></c></b></a> doesn't contain subnode c")
    }
    "match a node b contained inside a node a given its label and its attribute name" in {
      <a><b name="value"></b></a> must \("b", ("name"))
    }
    "match a node b contained inside a node a given its label and its attribute names" in {
      <a><b name="value" name2="value"></b></a> must \("b", ("name2", "name"))
    }
    "not match a node b contained inside a node a given its label and a missing attribute name" in {
      assertion(<a><b name2="value"></b></a> must \("b", "name")) must failWith("<a><b name2=\"value\"></b></a> doesn't contain subnode b with attributes: name")
    }
    "match a node contained in another given its label and its attributes and values" in {
      <a><b name="value"></b></a> must \("b", Map("name"->"value"))
    }
    "not match a node contained in another given its label and a wrong attribute or value" in {
      assertion(<a><b name="value"></b></a> must \("b", Map("name1"->"value"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name1=\"value\"")
      assertion(<a><b name="value"></b></a> must \("b", Map("name"->"value1"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value1\"")
    }
    "not match a node contained in another given its label and a inexisting attributes" in {
      assertion(<a><b name="value"></b></a> must \("b", Map("name"->"value", "name2"->"value2"))) must failWith("<a><b name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value\" name2=\"value2\"")
    }
    "not match a node contained in another given its label and a missing attributes" in {
      assertion(<a><b name="value" name2="value"></b></a> must \("b", Map("name"->"value"))) must failWith("<a><b name2=\"value\" name=\"value\"></b></a> doesn't contain subnode b with attributes: name=\"value\"")
    }
  }
  "\\ and \\\\ matchers" can {
    "be chained with \\ to provide full path searches" in {
      <a><b><c><d></d></c></b></a> must \("b").\("c")
    }
    "be chained with \\\\ to provide more full path searches" in {
      <a><b><c><d></d></c></b></a> must \\("b").\("c")
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
