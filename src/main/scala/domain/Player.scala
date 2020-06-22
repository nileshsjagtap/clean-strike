package domain
import core.StrikeUpdat._
import core.{DefunctCoin, Error, InvalidPlayerIds, MultiStrike, MultiWhiteStrike, RedStrike, Strike, StrikeUpdateUtil, StrikerStrike, WhiteStrike, `None`}

case class Player(id: Int, points: Int = 0, strike: List[Strike] = List.empty[Strike])

object Player {

  def apply(ids: List[Int]): Either[Error, List[Player]] = {
    for {
      validIds <- validatePlayerId(ids)
    } yield {
      validIds.map(Player(_))
    }
  }

  private def validatePlayerId(ids: List[Int]) = Either.cond(ids.distinct.size == ids.size && ids.size <= 4 && ids.size >= 2, ids, InvalidPlayerIds)

  def updatePlayer = (strike: Strike) => (player: Player) => {
    strike match {
      case Strike => update(1)(Strike)(add)(player)
      case MultiStrike => update(2)(MultiStrike)(add)(player)
      case RedStrike => update(3)(RedStrike)(add)(player)
      case StrikerStrike => update(1)(StrikerStrike)(subtract)(player)
      case DefunctCoin => update(2)(DefunctCoin)(subtract)(player)
      case WhiteStrike => update(2)(WhiteStrike)(add)(player)
      case MultiWhiteStrike => update(4)(MultiWhiteStrike)(add)(player)
      case `None` => update(0)(`None`)(add)(player)
    }
  }

  def updateOnStrike(strike: Strike, player: Player) = {
    import core.StrikeUpdat._
    StrikeUpdateUtil(strike, player)
  }

  def update = (points: Int) => (strike: Strike) => (f: (Int, Int) => Int) => (player: Player) =>
    player.copy(points = f(player.points, points), strike = player.strike.:+(strike))

  private def add(value1: Int, value2: Int) = value1 + value2

  private def subtract(value1: Int, value2: Int) = value1 - value2

}
