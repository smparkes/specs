/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.collection
import matcher.MatchersSpecification
import org.specs.collection.ExtendedIterable._
import org.specs.collection.ExtendedList._
import org.specs.runner._

class extendedIterableUnit extends IterableData with JUnit with ScalaCheck {
  "A sameElementsAs function" should returnTrue {
    "if 2 lists of lists contain the same elements in a different order" in {
      List(List(1), List(2, 3)) must haveSameElementsAs(List(List(3, 2), List(1)))
    }
    "if deeply nested lists have the same elements but in a different order" in {
      List(1, List(2, 3, List(4)), 5) must haveSameElementsAs(List(5, List(List(4), 2, 3), 1))
    }
    "when comparing xml nodes in a different order" in {
      <a> <b/> <c/> </a>.child must haveSameElementsAs(<a> <c/> <b/> </a>.child)
    }
    "for 2 iterables created with same elements in a different order" in {
      sameIterables must pass{
        t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
    "for 2 iterables created with same elements in a different order, even with different types like Stream and List" in {
      sameIterablesOfDifferentTypes must pass{
        t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
  }
  "A toDeepString function" should {
    "print the inside of an iterable, even if it is a Stream" in {
      Stream.cons(1, Stream.cons(2, Nil.toStream)).toDeepString must_== "[1, 2]"
    }
  }
  "A containsInOrder function" should {
    "check that one iterable is contained inside another one, in order" in {
      List(1, 2, 3).containsInOrder(1, 3) must beTrue
      List(1, 2, 3).containsInOrder(2, 1) must beFalse
    }
  }
  import org.scalacheck.Gen
  import scala.Math.min
  val sets = for {
        size1   <- Gen.choose(1, 3)
        set1    <- Gen.vectorOf(size1, Gen.elements("Art", "Bill", "Chris"))
        size2   <- Gen.choose(1, 3)
        set2   <- Gen.vectorOf(size2, Gen.elements("Ann", "Bess", "Clara"))
  } yield (Set(set1:_*), Set(set2:_*)) 

  "A combine function" should {
    "combine 2 sets returning the list of possible associations between the 2 sets" >> {
      "each list size must have the minimum size of both sets" in {
        sets must pass { s: (Set[String], Set[String]) => val (set1, set2) = s
          combine(set1, set2).forall(_ must have size(min(set1.size, set2.size)))
        }
      }
    }
    "combine 2 simplesets" in {
      val set1 = Set("Art", "Bill")
      val set2 = Set("Ann", "Bess")
      combine(set1, set2) must have the sameElementsAs(List(
        List(("Art", "Ann"), ("Bill", "Bess")),
        List(("Art", "Bess"), ("Bill", "Ann"))
      )) 
    }
  }

}
import org.specs._
import scalacheck.Gen._
import org.specs.ScalaCheck
import org.specs.collection.ExtendedIterable._
import org.specs.collection.ExtendedList._
import org.specs.Sugar._

trait IterableData extends Specification with Sugar with ScalaCheck {
  def returnTrue = addToSusVerb("return true")

  val sameIterables = for (i0 <- listOf(elements(1, 2, 3));
                           i1 <- listOf(elements(1, 4, 5, i0));
                           i2 <- listOf(elements(i0, i1, 2, 3));
                           val i3 = i2.scramble) yield (i2, i3)
  val sameIterablesOfDifferentTypes = for (i1 <- listOf(elements(1, 2, 3, listOf(elements(1, 2, 3)).toStream));
                                           val i2 = i1.scramble.toList) yield (i1.toStream, i2)

}
