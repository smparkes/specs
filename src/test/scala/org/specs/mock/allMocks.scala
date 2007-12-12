package org.specs.mock
import org.specs.runner._


object mocksUnit extends JUnit3(
    protocolsUnit)
object protocolsUnit extends Specification {
  "Mocks protocols" areSpecifiedBy (inAnyOrderUnit,
                                    inSequenceUnit,
                                    numberOfMessagesUnit,
                                    mockerUnit)
}

