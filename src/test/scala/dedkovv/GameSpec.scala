package dedkovv

import org.scalatest.{FreeSpec, Matchers}

class GameSpec extends FreeSpec with Matchers {
  "first" in {
    val g = new Game(5)

    g.rollInd shouldBe -1
    g.frameInd shouldBe 0
    g.frameToPins.isEmpty shouldBe true

    g.roll(3)
    g.rollInd shouldBe 0
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(3)

    g.roll(4)
    g.rollInd shouldBe 1
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(3, 4)

    g.roll(2)
    g.rollInd shouldBe 2
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2)

    g.roll(8)
    g.rollInd shouldBe 3
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)

    g.roll(10)
    g.rollInd shouldBe 4
    g.frameInd shouldBe 2
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)

    g.roll(10)
    g.rollInd shouldBe 5
    g.frameInd shouldBe 3
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)

    g.roll(1)
    g.rollInd shouldBe 6
    g.frameInd shouldBe 4
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)
    g.frameToPins(4) shouldBe Seq(1)

    g.roll(0)
    g.rollInd shouldBe 7
    g.frameInd shouldBe 4
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)
    g.frameToPins(4) shouldBe Seq(1, 0)
  }
}
