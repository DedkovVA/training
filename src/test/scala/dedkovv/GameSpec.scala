package dedkovv

import org.scalatest.{FreeSpec, Matchers}

class GameSpec extends FreeSpec with Matchers {
  "first" in {
    val g = new Game(5)

    g.getRollInd shouldBe -1
    g.getFrameInd shouldBe 0
    g.getFrameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

    g.roll(3)
    g.getRollInd shouldBe 0
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3)

    g.roll(4)
    g.getRollInd shouldBe 1
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    //7 in frame 0

    g.roll(2)
    g.getRollInd shouldBe 2
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2)

    g.roll(8)
    g.getRollInd shouldBe 3
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    //20 in frame 1

    g.roll(10)
    g.getRollInd shouldBe 4
    g.getFrameInd shouldBe 2
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    //21 in frame 2

    g.roll(10)
    g.getRollInd shouldBe 5
    g.getFrameInd shouldBe 3
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    //11 in frame 3

    g.roll(1)
    g.getRollInd shouldBe 6
    g.getFrameInd shouldBe 4
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    g.getFrameToPins(4) shouldBe Seq(1)

    g.roll(0)
    g.getRollInd shouldBe 7
    g.getFrameInd shouldBe 4
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    g.getFrameToPins(4) shouldBe Seq(1, 0)
    //1 in frame 4

    g.roll(10)
    g.getRollInd shouldBe 8
    g.getFrameInd shouldBe 5
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    g.getFrameToPins(4) shouldBe Seq(1, 0)
    g.getFrameToPins(5) shouldBe Seq(10)
    //30 in frame 5

    g.roll(10)
    g.getRollInd shouldBe 9
    g.getFrameInd shouldBe 5
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    g.getFrameToPins(4) shouldBe Seq(1, 0)
    g.getFrameToPins(5) shouldBe Seq(10, 10)

    g.isFinished shouldBe false

    g.roll(10)
    g.getRollInd shouldBe 10
    g.getFrameInd shouldBe 5
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    g.getFrameToPins(2) shouldBe Seq(10)
    g.getFrameToPins(3) shouldBe Seq(10)
    g.getFrameToPins(4) shouldBe Seq(1, 0)
    g.getFrameToPins(5) shouldBe Seq(10, 10, 10)

    g.isFinished shouldBe true

    g.score() shouldBe 90
  }

  "max 12 tries" in {
    val g = new Game
    Range(0, 10).indices.foreach {i  =>
      g.roll(10)

      g.getRollInd shouldBe i
      g.getFrameInd shouldBe i
      g.getFrameToPins(i) shouldBe Seq(10)
      g.isFinished shouldBe false
    }

    g.roll(10)
    g.getRollInd shouldBe 10
    g.getFrameInd shouldBe 9
    g.getFrameToPins(9) shouldBe Seq(10, 10)
    g.isFinished shouldBe false

    g.roll(10)
    g.getRollInd shouldBe 11
    g.getFrameInd shouldBe 9
    g.getFrameToPins(9) shouldBe Seq(10, 10, 10)
    g.isFinished shouldBe true

    g.score() shouldBe 300
  }

  "looser" in {
    val g = new Game(5)

    g.getRollInd shouldBe -1
    g.getFrameInd shouldBe 0
    g.getFrameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

    g.roll(0)
    g.getRollInd shouldBe 0
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(0)

    g.roll(0)
    g.getRollInd shouldBe 1
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(0, 0)

    g.roll(0)
    g.getRollInd shouldBe 2
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(0, 0)
    g.getFrameToPins(1) shouldBe Seq(0)

    g.roll(0)
    g.getRollInd shouldBe 3
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(0, 0)
    g.getFrameToPins(1) shouldBe Seq(0, 0)
  }

  "looser in loop" in {
    val g = new Game
    Range(0, 19).indices.foreach {i  =>
      g.roll(0)

      val n = i / 2

      g.getRollInd shouldBe i
      g.getFrameInd shouldBe n
      if (i % 2 == 0) {
        g.getFrameToPins(n) shouldBe Seq(0)
      } else {
        g.getFrameToPins(n) shouldBe Seq(0, 0)
      }
      g.isFinished shouldBe false
    }

    g.roll(0)
    g.getRollInd shouldBe 19
    g.getFrameInd shouldBe 9
    g.getFrameToPins(9) shouldBe Seq(0, 0)
    g.isFinished shouldBe true

    g.score() shouldBe 0
  }

  "spare at the end" in {
    val g = new Game(1)

    g.getRollInd shouldBe -1
    g.getFrameInd shouldBe 0
    g.getFrameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

    g.roll(3)
    g.getRollInd shouldBe 0
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3)

    g.roll(4)
    g.getRollInd shouldBe 1
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    //7 in frame 0

    g.roll(2)
    g.getRollInd shouldBe 2
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2)

    g.roll(8)
    g.getRollInd shouldBe 3
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8)
    //20 in frame 1

    g.roll(10)
    g.getRollInd shouldBe 4
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 8, 10)

    g.isFinished shouldBe true

    g.score() shouldBe 27
  }

  "fail at the end" in {
    val g = new Game(1)

    g.getRollInd shouldBe -1
    g.getFrameInd shouldBe 0
    g.getFrameToPins.isEmpty shouldBe true
    g.isFinished shouldBe false

    g.roll(3)
    g.getRollInd shouldBe 0
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3)

    g.roll(4)
    g.getRollInd shouldBe 1
    g.getFrameInd shouldBe 0
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    //7 in frame 0

    g.roll(2)
    g.getRollInd shouldBe 2
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2)

    g.roll(7)
    g.getRollInd shouldBe 3
    g.getFrameInd shouldBe 1
    g.getFrameToPins(0) shouldBe Seq(3, 4)
    g.getFrameToPins(1) shouldBe Seq(2, 7)
    //9 in frame 1

    g.isFinished shouldBe true

    g.score() shouldBe 16
  }

  "fail at the end via roll of list" in {
    val g = new Game()
    g.roll(List.fill(20)(4)) shouldBe 80
  }

  "fail at the end via roll of list 2" in {
    val g = new Game()
    g.roll(List.fill(21)(5)) shouldBe 150
  }
}
