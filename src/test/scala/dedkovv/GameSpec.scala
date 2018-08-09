package dedkovv

import org.scalatest.{FreeSpec, Matchers}

class GameSpec extends FreeSpec with Matchers {
  "first" in {
    val g = new Game(5)

    g.rollInd shouldBe -1
    g.frameInd shouldBe 0
    g.frameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

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

    g.roll(10)
    g.rollInd shouldBe 8
    g.frameInd shouldBe 5
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)
    g.frameToPins(4) shouldBe Seq(1, 0)
    g.frameToPins(5) shouldBe Seq(10)

    g.roll(10)
    g.rollInd shouldBe 9
    g.frameInd shouldBe 5
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)
    g.frameToPins(4) shouldBe Seq(1, 0)
    g.frameToPins(5) shouldBe Seq(10, 10)

    g.isFinished shouldBe false

    g.roll(10)
    g.rollInd shouldBe 10
    g.frameInd shouldBe 5
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8)
    g.frameToPins(2) shouldBe Seq(10)
    g.frameToPins(3) shouldBe Seq(10)
    g.frameToPins(4) shouldBe Seq(1, 0)
    g.frameToPins(5) shouldBe Seq(10, 10, 10)

    g.isFinished shouldBe true
  }

  "max 12 tries" in {
    val g = new Game
    Range(0, 10).indices.foreach {i  =>
      g.roll(10)

      g.rollInd shouldBe i
      g.frameInd shouldBe i
      g.frameToPins(i) shouldBe Seq(10)
      g.isFinished shouldBe false
    }

    g.roll(10)
    g.rollInd shouldBe 10
    g.frameInd shouldBe 9
    g.frameToPins(9) shouldBe Seq(10, 10)
    g.isFinished shouldBe false

    g.roll(10)
    g.rollInd shouldBe 11
    g.frameInd shouldBe 9
    g.frameToPins(9) shouldBe Seq(10, 10, 10)
    g.isFinished shouldBe true
  }

  "looser" in {
    val g = new Game(5)

    g.rollInd shouldBe -1
    g.frameInd shouldBe 0
    g.frameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

    g.roll(0)
    g.rollInd shouldBe 0
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(0)

    g.roll(0)
    g.rollInd shouldBe 1
    g.frameInd shouldBe 0
    g.frameToPins(0) shouldBe Seq(0, 0)

    g.roll(0)
    g.rollInd shouldBe 2
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(0, 0)
    g.frameToPins(1) shouldBe Seq(0)

    g.roll(0)
    g.rollInd shouldBe 3
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(0, 0)
    g.frameToPins(1) shouldBe Seq(0, 0)
  }

  "looser in loop" in {
    val g = new Game
    Range(0, 19).indices.foreach {i  =>
      g.roll(0)

      val n = i / 2

      g.rollInd shouldBe i
      g.frameInd shouldBe n
      if (i % 2 == 0) {
        g.frameToPins(n) shouldBe Seq(0)
      } else {
        g.frameToPins(n) shouldBe Seq(0, 0)
      }
      g.isFinished shouldBe false
    }

    g.roll(0)
    g.rollInd shouldBe 19
    g.frameInd shouldBe 9
    g.frameToPins(9) shouldBe Seq(0, 0)
    g.isFinished shouldBe true
  }

  "spare at the end" in {
    val g = new Game(1)

    g.rollInd shouldBe -1
    g.frameInd shouldBe 0
    g.frameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

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
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 8, 10)

    g.isFinished shouldBe true
  }

  "fail at the end" in {
    val g = new Game(1)

    g.rollInd shouldBe -1
    g.frameInd shouldBe 0
    g.frameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

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

    g.roll(7)
    g.rollInd shouldBe 3
    g.frameInd shouldBe 1
    g.frameToPins(0) shouldBe Seq(3, 4)
    g.frameToPins(1) shouldBe Seq(2, 7)

    g.isFinished shouldBe true
  }
}
