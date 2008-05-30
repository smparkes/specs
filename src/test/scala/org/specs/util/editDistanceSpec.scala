package org.specs.util

import org.specs._
import org.specs.runner._
import org.specs.matcher._

class editDistanceTest extends JUnit4(editDistanceSpec)
object editDistanceSpec extends Specification with EditDistance {
  "The show distance" should {
    "work on insertions" in {
      editDistance("kitte", "kittei") must_== 1
      showDistance("kitte", "kittei") must_== ("kitte", "kitte(i)")
      editDistance("kitten", "kittein") must_== 1
      showDistance("kitten", "kittein") must_== ("kitten", "kitte(i)n")
    }
    "work on suppressions" in {
     editDistance("kitten", "kit") must_== 3
     showDistance("kitten", "kit") must_== ("kit(ten)", "kit")
    }
    "work on substitutions" in {
     editDistance("kitten", "kitsin") must_== 2
     showDistance("kitten", "kitsin") must_== ("kit(te)n", "kit(si)n")
    }
  }
}
