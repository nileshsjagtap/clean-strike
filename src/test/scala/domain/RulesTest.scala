package domain

import core.{DefunctCoin, Draw, Strike, StrikerStrike, Winner, `None`}
import org.specs2.mutable.Specification

class RulesTest extends Specification {

  "Rules" should {

    "When a player does not pocket a coin for three successive turns he looses a point" in {
      val expected = Player(11, 4, List(Strike, StrikerStrike, DefunctCoin, core.`None`, core.`None`))
      val res = Rules.checkForSuccessiveFailure(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, core.`None`)))
      res must beEqualTo(expected)
    }

    "return same player if player has pocketed atleast one coin in last three strikes" in {
      val expected = Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike))
      val res = Rules.checkForSuccessiveFailure(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)))
      res must beEqualTo(expected)
    }

    "When a player fouls three times he looses a point" in {
      val expected = Player(11, 4, List(Strike, StrikerStrike, DefunctCoin, core.`None`, core.`None`))
      val res = Rules.checkForFoul(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, core.`None`)))
      res must beEqualTo(expected)
    }

    "return same player if player not lost a point more than two times" in {
      val expected = Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike))
      val res = Rules.checkForFoul(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)))
      res must beEqualTo(expected)
    }

    "return player as a Winner who have won at least 5 points and at least 3 points more than opponent" in {
      val expected = Some(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)))
      val res = Rules.checkWinner(List(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 1, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

    "return None if no player have won at least 5 points and at least 3 points more than opponent" in {
      val expected = scala.None
      val res = Rules.checkWinner(List(Player(11, 4, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 1, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

    "When the coins are exhausted return player as a WINNER who have won at least 5 points or at least 3 points more than opponent" in {
      val expected = Winner(11, List(5, 1))
      val res = Rules.checkResult(List(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 1, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

    "Declare the game as DRAW When the coins are exhausted and no player have won at least 5 points or at least 3 points more than opponent" in {
      val expected = Draw
      val res = Rules.checkResult(List(Player(11, 4, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 2, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

    "Return a Player as WINNER When the coins are exhausted and a player have won at least 5 points" in {
      val expected = Winner(11, List(5, 2))
      val res = Rules.checkResult(List(Player(11, 5, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 2, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

    "Return a Player as WINNER When the coins are exhausted and a player have won at least 3 points more than opponent" in {
      val expected = Winner(11, List(4, 1))
      val res = Rules.checkResult(List(Player(11, 4, List(Strike, StrikerStrike, DefunctCoin, Strike)), Player(21, 1, List(Strike, StrikerStrike, DefunctCoin, Strike))))
      res must beEqualTo(expected)
    }

  }

}
