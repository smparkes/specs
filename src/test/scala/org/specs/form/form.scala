package org.specs.form
import org.specs._

object formSpecifications extends Specification {
  "The form specifications" areSpecifiedBy (
    new fieldSpec,
    new formSpec,
    new propSpec,
    new propIterableSpec
  )
}


