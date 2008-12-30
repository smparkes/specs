package org.specs.collection
import org.specs.Specification
import org.specs.collection.ExtendedSeq._


object extendedSeqUnit extends Specification {
  "A mapFilter function" should {
    "map elements of a seq according to a function and filter the None results" in {
      def isEven(n: Int) = (n % 2) match {
        case 0 => Some(n)
        case _ => None
      }
      List(1, 2, 3, 4).mapFilter(isEven(_)) must_== List(2, 4)
    }
  }
}
