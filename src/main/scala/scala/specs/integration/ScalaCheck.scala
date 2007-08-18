package scala.specs.integration
import scalacheck._
import scalacheck.Gen._
import scalacheck.Prop._
import scalacheck.Test._

object sCheck extends ScalaCheck {
  implicit def combinedStringToString(c: CombinedString) = c.inner
  case class CombinedString(inner: String) {
    override def toString = inner
  }
}
trait ScalaCheck {
  
  def choose[T](l: T*) = elements(l.toList)
  def check[A1, P](prop: A1 => P)(implicit p: P => Prop, arb: Arbitrary[A1] => Gen[A1]) = Test.check(property(prop))
  def check[A1, A2, P](prop: (A1, A2) => P)
                      (implicit p: P => Prop, a1: Arbitrary[A1] => Gen[A1], 
                                              a2: Arbitrary[A2] => Gen[A2]) = Test.check(property(prop))
  def check[A1, A2, A3, P](prop: (A1, A2, A3) => P)
                          (implicit p: P => Prop, a1: Arbitrary[A1] => Gen[A1], 
                                                  a2: Arbitrary[A2] => Gen[A2], 
                                                  a3: Arbitrary[A3] => Gen[A3]) = Test.check(property(prop))
  /*  def check[A1, A2, A3, A4, P](prop: (A1, A2, A3, A4) => P)
  (implicit p: P => Prop, a1: Arbitrary[A1] => Gen[A1], 
                          a2: Arbitrary[A2] => Gen[A2], 
                          a3: Arbitrary[A3] => Gen[A3], 
                          a4: Arbitrary[A4] => Gen[A4]) = Test.check(property(prop))

                          def check[A1, A2, A3, A4, A5, P](prop: (A1, A2, A3, A4, A5) => P)
                          (implicit p: P => Prop, a1: Arbitrary[A1] => Gen[A1], 
                                                  a2: Arbitrary[A2] => Gen[A2], 
                                                  a3: Arbitrary[A3] => Gen[A3], 
                                                  a4: Arbitrary[A4] => Gen[A4],
                                                  a5: Arbitrary[A5] => Gen[A5]) = Test.check(property(prop))
def check[A1, A2, A3, A4, A5, A6, P](prop: (A1, A2, A3, A4, A5, A6) => P)
                              (implicit p: P => Prop, a1: Arbitrary[A1] => Gen[A1], 
                                                      a2: Arbitrary[A2] => Gen[A2], 
                                                      a3: Arbitrary[A3] => Gen[A3], 
                                                      a4: Arbitrary[A4] => Gen[A4],
                                                      a5: Arbitrary[A5] => Gen[A5],
                                                      a6: Arbitrary[A6] => Gen[A6]) = Test.check(property(prop))
*/

}
