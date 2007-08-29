package scala.specs
import scalacheck._
import scalacheck.Gen._

object ScalacheckExtension {
  def elementsOf[T](xs: List[T]): Gen[T] = if(xs.isEmpty) Gen.fail else 
    for { i <- choose((0,xs.length-1))
  } yield xs(i)
}
