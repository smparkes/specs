package scala.specs.mock
import scala.specs.integration._


object mocksUnit extends JUnit3(
    protocolsUnit)
object protocolsUnit extends Specification {
  "Mocks protocols" areSpecifiedBy (inAnyOrderUnit,
                                    inSequenceUnit,
                                    numberOfMessagesUnit)
}

