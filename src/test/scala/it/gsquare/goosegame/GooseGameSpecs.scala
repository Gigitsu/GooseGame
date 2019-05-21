package it.gsquare.goosegame

import org.scalatest.{FlatSpec, Matchers}

/**
  * created by gigitsu on 21/05/2019.
  */
class GooseGameSpecs extends FlatSpec with Matchers {

  behavior of "GooseGame"
  val gg = new GooseGame()

  it should "add new players" in {
    val (b1, msg1) = gg.addPlayer(Board(), "playerOne")
    val (b2, msg2) = gg.addPlayer(b1, "playerTwo")

    msg1 shouldBe "Players: playerOne"
    b1 should contain ("playerOne" -> 0)

    msg2 shouldBe "Players: playerOne, playerTwo"
    b2 should contain ("playerOne" -> 0)
    b2 should contain ("playerTwo" -> 0)
  }

  it should "not add an existing players" in {
    val (b1, msg1) = gg.addPlayer(Board("playerOne" -> 0), "playerOne")

    msg1 shouldBe "playerOne: already existing player"
    b1 should contain ("playerOne" -> 0)
  }

  it should "do nothing if is passed invalid dice value" in {
    val (b1, msg1) = gg.movePlayer(Board(), "playerOne", 7, 1)
    val (b2, msg2) = gg.movePlayer(Board(), "playerOne", 2, 9)
    val (b3, msg3) = gg.movePlayer(Board(), "playerOne", 10, 21)

    msg1 shouldBe "Invalid dice values [7, 1]"
    b1 shouldBe empty

    msg2 shouldBe "Invalid dice values [2, 9]"
    b2 shouldBe empty

    msg3 shouldBe "Invalid dice values [10, 21]"
    b3 shouldBe empty
  }

  it should "do nothing if is passed a name of player that does not exists" in {
    val (b1, msg1) = gg.movePlayer(Board(), "playerOne", 1, 3)

    msg1 shouldBe "playerOne: not existing player"

    b1 shouldBe empty
  }

  it should "correctly move a player" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 20), "playerOne", 2, 3)
    val (b2, msg2) = gg.movePlayer(Board("playerOne" -> 20), "playerOne")

    msg1 shouldBe "playerOne rolls 2, 3. playerOne moves from 20 to 25."
    b1 should contain ("playerOne" -> 25)

    b2("playerOne") should be > 20
    b2("playerOne") should be < 33
  }

  it should "jumps on the bridge" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 0), "playerOne", 4, 2)

    msg1 shouldBe "playerOne rolls 4, 2. playerOne moves from Start to The Bridge. playerOne jumps to 12."
    b1 should contain ("playerOne" -> 12)
  }

  it should "move again if land on The Goose" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 1), "playerOne", 2, 2)
    val (b2, msg2) = gg.movePlayer(Board("playerOne" -> 7), "playerOne", 1, 1)
    val (b3, msg3) = gg.movePlayer(Board("playerOne" -> 12), "playerOne", 1, 1)
    val (b4, msg4) = gg.movePlayer(Board("playerOne" -> 16), "playerOne", 1, 1)
    val (b5, msg5) = gg.movePlayer(Board("playerOne" -> 21), "playerOne", 1, 1)
    val (b6, msg6) = gg.movePlayer(Board("playerOne" -> 25), "playerOne", 1, 1)

    msg1 shouldBe "playerOne rolls 2, 2. playerOne moves from 1 to 5, The Goose. playerOne moves again and goes to 9, The Goose. playerOne moves again and goes to 13."
    b1 should contain ("playerOne" -> 13)

    msg2 shouldBe "playerOne rolls 1, 1. playerOne moves from 7 to 9, The Goose. playerOne moves again and goes to 11."
    b2 should contain ("playerOne" -> 11)

    msg3 shouldBe "playerOne rolls 1, 1. playerOne moves from 12 to 14, The Goose. playerOne moves again and goes to 16."
    b3 should contain ("playerOne" -> 16)

    msg4 shouldBe "playerOne rolls 1, 1. playerOne moves from 16 to 18, The Goose. playerOne moves again and goes to 20."
    b4 should contain ("playerOne" -> 20)

    msg5 shouldBe "playerOne rolls 1, 1. playerOne moves from 21 to 23, The Goose. playerOne moves again and goes to 25."
    b5 should contain ("playerOne" -> 25)

    msg6 shouldBe "playerOne rolls 1, 1. playerOne moves from 25 to 27, The Goose. playerOne moves again and goes to 29."
    b6 should contain ("playerOne" -> 29)
  }

  it should "win when reach cell 63 and generate a new board" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 60), "playerOne", 2, 1)

    msg1 shouldBe "playerOne rolls 2, 1. playerOne moves from 60 to 63. playerOne wins!!\n\nA new board has been generated. Let's play again!"
    b1 shouldBe empty
  }

  it should "win with exact dice shooting" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 60), "playerOne", 2, 3)

    msg1 shouldBe "playerOne rolls 2, 3. playerOne moves from 60 to 63. playerOne bounces! playerOne returns to 61."
    b1 should contain ("playerOne" -> 61)
  }

  it should "prank a player" in {
    val (b1, msg1) = gg.movePlayer(Board("playerOne" -> 30, "playerTwo" -> 34), "playerOne", 1, 3)

    msg1 shouldBe "playerOne rolls 1, 3. playerOne moves from 30 to 34. On 34 there is playerTwo, who returns to 30."

    b1 should contain ("playerOne" -> 34)
    b1 should contain ("playerTwo" -> 30)
  }
}
