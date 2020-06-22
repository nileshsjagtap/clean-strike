package domain

import core.{Black, Error, InvalidBlackCoinCount, InvalidRedCoinCount, InvalidWhiteCoinCount, MultiStrike, MultiWhiteStrike, Red, RedStrike, Strike, StrikerStrike, White, WhiteStrike, `None`}
import org.specs2.mutable.Specification

class CarromBoardTest extends Specification {

  "CarromBoard" should{

    "return carromBoard if black coin , red coin and white coin count is valid" in {
      val expected = CarromBoard(List(Black,Black,Black,Red, White, White))
      val response = CarromBoard.apply(3, 1, 2)
      response must beRight(expected)
    }

    "not return carromBoard if black coin count is invalid" in {
      val expected: Error = InvalidBlackCoinCount
      val response = CarromBoard.apply(0, 1, 5)
      response must beLeft(expected)
    }

    "not return carromBoard if red coin count is invalid" in {
      val expected: Error = InvalidRedCoinCount
      val response = CarromBoard.apply(3, 0, 5)
      response must beLeft(expected)
    }

    "remove one black coin if player performs strike" in {
      val expected = CarromBoard(List(Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(Strike)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "remove one white coin if player performs white strike" in {
      val expected = CarromBoard(List(Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(WhiteStrike)(CarromBoard(List(Black, Black, Black, Red, White)))
      res must beEqualTo(expected)
    }

    "remove two white coins if player performs multi white strike" in {
      val expected = CarromBoard(List(Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(MultiWhiteStrike)(CarromBoard(List(Black, Black, Black, Red, White, White)))
      res must beEqualTo(expected)
    }

    "remove two black coins if player performs multi-strike" in {
      val expected = CarromBoard(List(Black,Black, Red))
      val res = CarromBoard.updateCarromBoard(MultiStrike)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "remove one Red coin if player performs RedStrike" in {
      val expected = CarromBoard(List(Black,Black,Black,Black))
      val res = CarromBoard.updateCarromBoard(RedStrike)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "remove one black coin if player performs DefunctCoin" in {
      val expected = CarromBoard(List(Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(Strike)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "return same carromBoard if player performs StrikerStrike" in {
      val expected = CarromBoard(List(Black,Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(StrikerStrike)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "return same carromBoard if player performs None" in {
      val expected = CarromBoard(List(Black,Black,Black,Black,Red))
      val res = CarromBoard.updateCarromBoard(`None`)(CarromBoard(List(Black, Black, Black, Black, Red)))
      res must beEqualTo(expected)
    }

    "not return carrom board if white coins count is not valid" in {
      val expected: Error = InvalidWhiteCoinCount
      val response = CarromBoard.apply(5, 1, 0)
      response must beLeft(expected)
    }


  }

}
