package org.specs.util
import org.specs._

object utilSpecifications extends Specification {
    "The util specifications" areSpecifiedBy (
        new classSpec,
        new editDistanceSpec,
		new extendedStringSpec,
        new includeExcludeSpec,
        new scalaInterpreterSpec,
        new timerSpec
    )
}

object utilUnits extends Specification {
  "The unit tests for the util package" areSpecifiedBy (
      new dataRowUnit,
      new dataTableHeaderUnit,
      new dataTableUnit,
	  new extendedThrowableUnit)
}

