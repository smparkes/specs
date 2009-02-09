package org.specs.specification
import org.scalacheck.Gen.{ choose, sized, vectorOf }
import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.specs.matcher._

trait SpecificationGenerator { self: Specification =>
  object spec extends Specification("generated spec")

  def genExpectation = for(value <- choose(0, 4)) yield { () =>
    value match {
      case 0 | 1 => spec.theValue(value) must_== 1
      case 2     => spec.skip("this is a skipped example")
      case 3     => error("error in the example")
    }
  }

  def genExample(sus: Sus) = for (a <- genExpectation) yield {
    val newExample = new Example("generated example", sus)
    sus.addExample(newExample)
    newExample.in { a() }
  }

  def genSizedSus(size: Int): Gen[Sus] = genSizedSus(size, spec)
  def genSizedSus(size: Int, s: Specification): Gen[Sus] = {
    val sus = new Sus("sus with " + size + " max examples", s)
    for { n <- choose(0, size)
          e <- vectorOf(n, genExample(sus))
    } yield sus
  }
  def genSus = sized(size => genSizedSus(size))
  def genSizedSpec(size: Int): Gen[Specification] = {
    val generatedSpec = new Specification("spec with " + size + " max sus") {}
    for { systemsNb <- choose(0, size)
          systems <- vectorOf(systemsNb, genSizedSus(size, generatedSpec))
          subSpecsNb <- choose(0, 1)
          subSpecs <- vectorOf(subSpecsNb, genSizedSpec(size))
    } yield {
      subSpecs.foreach(generatedSpec.include(_))
      generatedSpec
    }
  }
  def genSpec = sized(size => genSizedSpec(size))

  implicit val arbitrarySus: Arbitrary[Sus] = Arbitrary { genSus }
  implicit val arbitrarySpec: Arbitrary[Specification] = Arbitrary { genSpec }
}
object generatorSpec extends Specification with SpecificationGenerator with ScalaCheck {
  "a sus" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      forAll {(sus: Sus) =>
        (sus.failures.size + sus.errors.size + sus.skipped.size + sus.successes.size) must be_==(sus.examples.size)
      } must pass(set(maxSize -> 5))
    }
  }
  "a specification" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      forAll {(spec: Specification) =>
        (spec.failures.size + spec.errors.size + spec.skipped.size + spec.successes.size) must be_==(spec.examples.size)
      } must pass(set(maxSize -> 5))
    }
  }
}
