package domain


import core.{DefunctCoin, Error, InvalidStrike, MultiStrike, RedStrike, Strike, StrikerStrike, WhiteStrike, `None`}
import org.specs2.mutable.Specification

class StrikeTest extends Specification {

  "Strike" should {
    "return list of strikes if input is valid strike" in {
      val input = List("Strike", "Multi-strike", "Red strike", "Striker strike", "Defunct coin", "None", "White Strike")
      val expected: List[Strike] = List(Strike, MultiStrike, RedStrike, StrikerStrike, DefunctCoin, None, WhiteStrike)
      val res = Strike(input)
      res must beRight(expected)
    }

    "return InvalidStrike if input is invalid strike" in {
      val input = List("Strike", "Multi-strike", "Redstrike", "Strikerstrike", "Defunct coin", "None")
      val expected: Error = InvalidStrike
      val res = Strike(input)
      res must beLeft(expected)
    }
  }

}
