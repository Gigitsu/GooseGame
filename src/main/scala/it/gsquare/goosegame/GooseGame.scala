package it.gsquare.goosegame

import scala.language.postfixOps

import scala.io.Source.stdin
import scala.util.matching.Regex
import scala.util.Random

/**
  * created by gigitsu on 19/05/2019.
  */
class GooseGame {
  private val diceValueRange: Range.Inclusive = 1 to 6
  private val diceTosser: Iterator[Int] = Stream continually (Random.nextInt(6) + 1) iterator

  def addPlayer(board: Board, name: String): BoardWithMessage = {
    if (board.contains(name)) {
      board -> s"$name: already existing player"
    } else {
      board.updated(name, 0) -> s"Players: ${(board.keys.toSeq :+ name).mkString(", ")}"
    }
  }

  def movePlayer(board: Board, name: String): BoardWithMessage = movePlayer(board, name, diceTosser.next, diceTosser.next)

  def movePlayer(board: Board, name: String, x1: Int, x2: Int): BoardWithMessage = {
    if (!(x1 :: x2 :: Nil).forall(diceValueRange.contains)) {
      board -> s"Invalid dice values [$x1, $x2]"
    } else if (board.contains(name)) {
      val steps = x1 + x2

      val from = board(name)
      val to = from + steps

      val initialMsg = s"$name rolls $x1, $x2. $name moves from ${positionName(from)} to ${positionName(to)}."
      val (finishingLine, msg) = continueMove(name, to, steps, initialMsg)

      // last cell reached
      if (finishingLine == 63) Board() -> s"$msg\n\nA new board has been generated. Let's play again!"
      else board.find { case (n, p) => n != name && p == finishingLine } match {
        case Some((pranked, _)) =>
          board.updated(name, finishingLine).updated(pranked, from) -> s"$msg On $finishingLine there is $pranked, who returns to $from."
        case None =>
          board.updated(name, finishingLine) -> msg
      }
    } else {
      board -> s"$name: not existing player"
    }
  }

  def continueMove(name: String, from: Int, steps: Int, msg: String = ""): (Int, String) = from match {
    case 6 => 12 -> s"$msg $name jumps to 12."
    case 63 => 63 -> s"$msg $name wins!!"
    case 5 | 9 | 14 | 18 | 23 | 27 =>
      val to = from + steps
      continueMove(name, to, steps, s"$msg $name moves again and goes to ${positionName(to)}.")
    case x if x > 63 =>
      val to = 63 * 2 - x
      to -> s"$msg $name bounces! $name returns to $to."
    case x => x -> msg
  }

  def positionName(p: Int): String = p match {
    case 0 => "Start"
    case 5 | 9 | 14 | 18 | 23 | 27 => s"$p, The Goose"
    case 6 => "The Bridge"
    case x => Math.min(x, 63).toString
  }
}

object GooseGame extends App {
  object commands {
    val AddPlayer: Regex = """add player (\w+)""".r
    val MovePlayer: Regex = """move (\w+) (\d+),\s?(\d+)""".r
    val TossAndMovePlayer: Regex = """move (\w+)""".r
  }

  private val gg: GooseGame = new GooseGame

  implicit def boardWithMessageToBoard(bm: BoardWithMessage): Board = bm match {
    case (board, msg) =>
      println(msg)
      board
  }

  stdin.getLines().takeWhile(_ != "quit")
    .foldLeft(Board()) {
      case (board, commands.AddPlayer(name)) => gg.addPlayer(board, name)
      case (board, commands.MovePlayer(name, x1, x2)) => gg.movePlayer(board, name, x1.toInt, x2.toInt)
      case (board, commands.TossAndMovePlayer(name)) => gg.movePlayer(board, name)
      case (board, _) =>
        println("Command not recognized.")
        board
    }
}
