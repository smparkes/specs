package scala.collection
import org.specs.runner._
import org.specs.Sugar._
import org.specs._
import org.collection.ExtendedIterable._
import org.collection.ExtendedList._
import scalacheck.Gen._
import org.specs.matcher.ScalacheckParameters._

object extendedIterableUnitSuite extends JUnit3(extendedIterableUnit)
object extendedIterableUnit extends Specification with Sugar {
  "A toDeepString function" should {
    "print the inside of an iterable, even if it is a Stream" in {
			Stream.cons(1, Stream.cons(2, Nil.toStream)).toDeepString must_== "[1, 2]"
    }
  }
  "A sameElementsAs function" should {
    val sameIterables = for (i1 <- listOf(elements(1, 2, 3, listOf(elements(1, 2, 3))));
                             val i2 = i1.scramble)
                          yield (i1, i2)
    "return true if the 2 iterables are the same" in {
      sameIterables must pass { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
    "return true if the 2 iterables are the same, even with a stream and a list" in {
      val sameIterables = for (i1 <- listOf(elements(1, 2, 3, listOf(elements(1, 2, 3)).toStream));
                            val i2 = i1.scramble.toList)
                            yield (i1.toStream, i2)
      sameIterables must pass { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
  }
}
