package domain

import core._
import org.specs2.mutable.Specification

class GameTest extends Specification {

  "Game" should {
    "check result when the turns are empty" in {
      val turns = List.empty[Turn]
      val players = List(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 1, List(Strike, StrikerStrike, DefunctCoin, Strike)))
      val carromBoard = CarromBoard(List(Black, Black, Red))
      val expected = Winner(11, List(5, 1))
      val res = Game.start(Game(turns, players, carromBoard), turns)
      res must beEqualTo(expected)
    }

    "check WINNER before playing next turn" in {
      val turns = List(Turn(0, Strike),Turn(1, Strike),Turn(0, MultiStrike),Turn(1, Strike),Turn(0, RedStrike),Turn(1, Strike))
      val players = List(Player(11, 0, List.empty[Strike]), Player(21, 0, List.empty[Strike]))
      val carromBoard = CarromBoard(List(Black, Black, Black, Black, Black, Black, Red, White))
      val expected = Winner(11, List(6, 2))
      val res = Game.start(Game(turns, players, carromBoard), turns)
      res must beEqualTo(expected)
    }

    "skip the turn if particular coin is not available on carromBoard" in {
      val turns = List(Turn(0, Strike),Turn(1, Strike),Turn(0, RedStrike),Turn(1, Strike),Turn(0, RedStrike),Turn(1, Strike))
      val players = List(Player(11, 0, List.empty[Strike]), Player(21, 0, List.empty[Strike]))
      val carromBoard = CarromBoard(List(Black, Black, Black, Black, Black, Black, Red))
      val expected = Draw
      val res = Game.start(Game(turns, players, carromBoard), turns)
      res must beEqualTo(expected)
    }
  }

}
