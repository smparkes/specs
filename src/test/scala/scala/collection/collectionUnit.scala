package scala.collection
import scala.specs._

object collectionUnit extends Specification {
    "The collection unit tests" areSpecifiedBy (
        extendedIterableUnit,  
        extendedListUnit)
}
