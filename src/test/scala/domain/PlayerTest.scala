package domain

import core.{DefunctCoin, Error, InvalidPlayerIds, MultiStrike, MultiWhiteStrike, RedStrike, Strike, StrikerStrike, WhiteStrike, `None`}
import org.specs2.mutable.Specification

class PlayerTest extends Specification {

  "Player" should {
    "create player if provided ids are valid" in {
      val expected = List(Player(11, 0, List.empty[Strike]), Player(21, 0, List.empty[Strike]))
      val res = Player.apply(List(11, 21))
      res must beRight(expected)
    }

    "not create player if provided ids are duplicate" in {
      val expected: Error = InvalidPlayerIds
      val res = Player.apply(List(11, 11))
      res must beLeft(expected)
    }

    "not create player if provided ids are less than 2" in {
      val expected: Error = InvalidPlayerIds
      val res = Player.apply(List(11))
      res must beLeft(expected)
    }

    "create player if provided ids are more than 4" in {
      val expected: Error = InvalidPlayerIds
      val res = Player.apply(List(11, 21, 31, 41, 51))
      res must beLeft(expected)
    }

    "increment one point of player if player performs strike" in {
      val expected = Player(11, 1, List(Strike))
      val res = Player.updatePlayer(Strike)(Player(11, 0, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "increment two points of player if player performs Whitestrike" in {
      val expected = Player(11, 2, List(WhiteStrike))
      val res = Player.updatePlayer(WhiteStrike)(Player(11, 0, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "increment four points of player if player performs MultiWhitestrike" in {
      val expected = Player(11, 4, List(MultiWhiteStrike))
      val res = Player.updatePlayer(MultiWhiteStrike)(Player(11, 0, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "increment two points of player if player performs multi-strike" in {
      val expected = Player(11, 2, List(MultiStrike))
      val res = Player.updatePlayer(MultiStrike)(Player(11, 0, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "increment three points of player if player performs RedStrike" in {
      val expected = Player(11, 3, List(RedStrike))
      val res = Player.updatePlayer(RedStrike)(Player(11, 0, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "decrement one point of player if player performs StrikerStrike" in {
      val expected = Player(11, 0, List(StrikerStrike))
      val res = Player.updatePlayer(StrikerStrike)(Player(11, 1, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "decrement two points of player if player performs StrikerStrike" in {
      val expected = Player(11, 0, List(DefunctCoin))
      val res = Player.updatePlayer(DefunctCoin)(Player(11, 2, List.empty[Strike]))
      res must beEqualTo(expected)
    }

    "return same player if player performs `None`" in {
      val expected = Player(11, 2, List(MultiStrike, `None`))
      val res = Player.updatePlayer(`None`)(Player(11, 2, List(MultiStrike)))
      res must beEqualTo(expected)
    }
  }

}
