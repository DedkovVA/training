package dedkovv

import org.scalatest.{FreeSpec, Matchers}

class BootSpec extends FreeSpec with Matchers {
  Boot.one shouldBe 1
}
