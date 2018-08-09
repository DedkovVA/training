package dedkovv

import org.scalatest.{FreeSpec, Matchers}

class GameSpec extends FreeSpec with Matchers {
  "first" in {
    val g = new Game

    g.roll(3)
    g.rollInd shouldBe 0
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(3)

    g.roll(4)
    g.rollInd shouldBe 1
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(3, 4)
  }
}
