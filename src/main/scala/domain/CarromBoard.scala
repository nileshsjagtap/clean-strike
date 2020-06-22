package domain

import core.{Black, Coin, DefunctCoin, Error, InvalidBlackCoinCount, InvalidRedCoinCount, InvalidWhiteCoinCount, MultiStrike, MultiWhiteStrike, Red, RedStrike, Strike, StrikerStrike, White, WhiteStrike, `None`}

case class CarromBoard(coins: List[Coin])

object CarromBoard {

  def apply(blackCoinCount: Int, redCointCount: Int, whiteCoinCount: Int): Either[Error, CarromBoard] = {
    for {
      blackCoins <- validateBlackCoinCount(blackCoinCount)
      redCoins <- validateRedCoinCount(redCointCount)
      whiteCoins <- validateWhiteCoinCount(whiteCoinCount)
    } yield CarromBoard(blackCoins ++ redCoins ++ whiteCoins)
  }

  def updateCarromBoard = (strike: Strike) => (carromBoard: CarromBoard) => strike match {
    case Strike => updateCoins(carromBoard)(List(Black))(remove)
    case MultiStrike => updateCoins(carromBoard)(List(Black, Black))(remove)
    case RedStrike => updateCoins(carromBoard)(List(Red))(remove)
    case DefunctCoin => updateCoins(carromBoard)(List(Black))(remove)
    case WhiteStrike => updateCoins(carromBoard)(List(White))(remove)
    case MultiWhiteStrike => updateCoins(carromBoard)(List(White,White))(remove)
    case StrikerStrike | `None` => carromBoard
  }

  private def validateBlackCoinCount(blackCoinCount: Int)=
    Either.cond(blackCoinCount > 0 && blackCoinCount < 14, List.fill(blackCoinCount)(Black), InvalidBlackCoinCount)

  private def validateRedCoinCount(redCointCount: Int) =
    Either.cond(redCointCount > 0, List.fill(redCointCount)(Red), InvalidRedCoinCount)

  private def validateWhiteCoinCount(whiteCoinCount: Int) =
    Either.cond(whiteCoinCount > 0, List.fill(whiteCoinCount)(White), InvalidWhiteCoinCount)

  private def updateCoins = (carromBoard: CarromBoard) => (coins: List[Coin]) => (f: (List[Coin], List[Coin]) => List[Coin]) =>
    carromBoard.copy(coins = f(carromBoard.coins, coins))

  def hasCoin(carromBoard: CarromBoard, strike: Strike) = strike match {
    case Strike | DefunctCoin | StrikerStrike | `None` => carromBoard.coins.count(List(Black).contains(_)) >= 1
    case RedStrike => carromBoard.coins.count(List(Red).contains(_)) >= 1
    case MultiStrike => carromBoard.coins.count(List(Black).contains(_)) >= 2
  }

  private def remove(coinsOnCarromBoard: List[Coin], coinsToRemove: List[Coin]) = coinsOnCarromBoard diff coinsToRemove

}



