package scala.specs
import scala.specs.integration._
import scala.specs.Sugar._

object mockerUnitSuite extends JUnit3TestSuite(mockerUnit)
object mockerUnit extends Specification with Sugar with ProtocolTypes {
  "A mocker" should {
    object mocker extends Mocker
    usingBefore {() => mocker.protocol.clear}

    "create a protocol when expecting calls" in {
      val protocol = mocker.expect {}
      protocol mustNotBe null
    }
    "add a new expected call when recording calls" in {
      val mock = new Object { def method = mocker.record }
      val protocol = mocker.expect { mock.method }
      
      protocol verifies(_.isSpecified)
      protocol.protocolDef mustNotBe null
      protocol.protocolDef.expectedCalls must beLike { case List(ExpectedCall(_)) => ok }
    }
    "add a new received call when receiving calls" in {
      val mock = new Object { def method = mocker.record }
      val protocol = mocker.expect { mock.method }
      mock.method

      protocol.receivedCalls must beLike { case List(ReceivedCall(_)) => ok }
    }
    "have a failure when not receiving an expected call" in {
      val mock = new Object { def method = mocker.record }
      val protocol = mocker.expect { mock.method }

      protocol.failures must beMatching("Expected in any order .*. Received none")
    }
    "include a protocol def inside a protocol def if expectations are nested" in {
      val mock = new Object { def a = mocker.record; def b = mocker.record; def c = mocker.record }
      val protocol = mocker.expect {
        mock.a
        mocker.expect { mock.b } 
        mock.c
      }
      protocol.protocolDef must beLike { case ProtocolDef(inAnyOrder, List(ExpectedCall(_), 
                                                                           ProtocolDef(_, _),
                                                                           ExpectedCall(_))) => ok }
    }
    "accept nested protocol defs using different protocol types: anyOf 1.of{method}; 2.of {method}" in {
      val mock = new Object { def a = mocker.record; def b = mocker.record; def c = mocker.record }
      val protocol = mocker.expect {
        mocker.expect(oneOf) { mock.a } 
        mocker.expect(twoOf) { mock.b } 
      }
      protocol.protocolDef must beLike { case ProtocolDef(inAnyOrder, List(ProtocolDef(x, _),
                                                                           ProtocolDef(y, _))) => (x, y) == (oneOf, twoOf) }
    }
  }
}
object inAnyOrderUnitSuite extends JUnit3TestSuite(inAnyOrderUnit)
object inAnyOrderUnit extends Specification with ProtocolTypes {
  "A protocol type 'inAnyOrder'" should {
    val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
    val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
    val exactly2Calls = new ProtocolDef(twoOf, List(e))
    "consume nothing if exp=m and rec=nil" in {
      inAnyOrder.consume((e), ()) must_== (List(e), Nil)
    }
    "consume all if exp=m and rec=m" in {
      inAnyOrder.consume((e), (r)) must_== (Nil, Nil)
    }
    "consume one exp if exp=m1, m2 and rec=m2" in {
      inAnyOrder.consume(List(e1, e2), (r2)) must_== (List(e1), Nil)
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inAnyOrder.consume(List(e1, e2), List(r2, r1)) must_== (Nil, Nil)
    }
    "consume only one received message if exp=m and rec=m, m" in {
      inAnyOrder.consume(List(e), List(r, r)) must_== (Nil, List(r))
    }
    "find the shortest sequence of received calls that can be expected" in {
      inAnyOrder.findShortestExpectedSequence(List(exactly2Calls), List(r, r)) must_== List(r, r)
    }
  }
}
object inSequenceSuite extends JUnit3TestSuite(inSequenceUnit)
object inSequenceUnit extends Specification with ProtocolTypes {
  "A protocol type 'inSequence'" should {
    val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
    val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
    "consume all if exp=m and rec=m" in {
      inSequence.consume((e), (r)) must_== (Nil, Nil)
    }
    "consume the first exp if exp=m1, m2 and rec=m1" in {
      inSequence.consume(List(e1, e2), (r1)) must_== (List(e2), Nil)
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inSequence.consume(List(e1, e2), List(r1, r2)) must_== (Nil, Nil)
    }
  }
}
object numberOfMessagesSuite extends JUnit3TestSuite(numberOfMessagesUnit)
object numberOfMessagesUnit extends Specification with ProtocolTypes {
  "A protocol type 'numberOfMessages'" should {
    val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
    val (r, r1, r2) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"))
    "exactly 1: consume nothing if exp=m and rec=nil" in {
      new numberOfMessages(exactlyN(1)).consume((e), ()) must_== (List(e), Nil)
    }
    "exactly 1: consume all if exp=m and rec=m" in {
      new numberOfMessages(exactlyN(1)).consume((e), (r)) must_== (Nil, Nil)
    }
    "exactly 2: consume all if exp=m and rec=m, m" in {
      new numberOfMessages(exactlyN(2)).consume(List(e), List(r, r)) must_== (Nil, Nil)
    }
    "exactly 2: consume nothing if exp=m and rec=m" in {
      new numberOfMessages(exactlyN(2)).consume(List(e), List(r)) must_== (List(e), List(r))
    }
    "exactly 2 of m expects m, m" in {
      ProtocolDef(twoOf, List(e)).expects(List(r, r)) mustBe true
    }
  }
}
object listUtilsUnitSuite extends JUnit3TestSuite(listUtilsUnit)
object listUtilsUnit extends Specification with Sugar with ListUtils {
  "A removeFirst predicate function" should {
    "remove nothing if the list is empty" in {
      val l: List[String] = Nil
      l.removeFirst(_ == "a") must_== Nil
    }
    "remove the first element if the list contains it" in {
      val l: List[String] = List("a", "b", "c", "b")
      l.removeFirst(_ == "b") must_== List("a", "c", "b")
    }
  }
  "A removeFirstSeq with a list parameter function" should {
    "remove nothing if the list is empty" in {
      val l: List[String] = Nil
      l.removeFirstSeq(List()) must_== Nil
    }
    "remove the first elemets if the list starts with the sublist" in {
      val l: List[String] = List("a", "b", "c", "b", "c")
      l.removeFirstSeq(List("a", "b")) must_== List("c", "b", "c")
    }
    "remove the first subsequence corresponding to the list parameter" in {
      val l: List[String] = List("a", "b", "c", "b", "c")
      l.removeFirstSeq(List("b", "c")) must_== List("a", "b", "c")
    }
  }
  "A function returning every order of a list" should {
    "return a list with one permutations for a list with one element" in {
      val l = List("a")
      everyOrder(l) must beLike { case List(List("a")) => ok }
    }
    "return a list of 2 permutations for a list with two elements" in {
      val l = List("a", "b")
      everyOrder(l) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "return a list of 6 permutations for a list with 3 elements" in {
      val l = List("a", "b", "c")
      everyOrder(l) must (contain(List("a", "b", "c")) and 
                                     contain(List("c", "b", "a")) and
                                     contain(List("c", "a", "b")) and
                                     contain(List("b", "c", "a")) and
                                     contain(List("b", "a", "c")) and
                                     contain(List("a", "c", "b")))
    }
  }
  "A function mixing an element with a list" should {
    "create 2 couples with a list of one element" in {
      val l = List("b")
      mix("a", l) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "create 3 lists with a list of 2 elements" in {
      val l = List("b", "c")
      mix("a", l) must beLike { case List(List("a", "b", "c"), 
                                                     List("b", "a", "c"),
                                                     List("b", "c", "a") ) => ok }
    }
  }
}