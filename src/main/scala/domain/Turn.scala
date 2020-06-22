package domain

import core.{Error, InvalidPlayersAndStrikes, Strike}

case class Turn(playerIdx: Int, strike: Strike)

object Turn {

  def apply(players: List[Player], strikes: List[Strike]): Either[Error, List[Turn]] = {
    if (players.isEmpty && strikes.isEmpty)
      Left(InvalidPlayersAndStrikes)
    else {
      Right(for {
        strikesSeq <- strikes.grouped(players.size).toList
        (idx, strike) <- players.indices.toList zip strikesSeq
      } yield Turn(idx, strike))
    }
  }
}
