package domain

import core.{DefunctCoin, Draw, StrikerStrike, Winner}
import domain.Player.update

object Rules {

  def checkRules = (player: Player) => (checkForSuccessiveFailure andThen checkForFoul) (player)

  def checkForSuccessiveFailure = (player: Player) => {
    if (player.strike.takeRight(3).forall(List(StrikerStrike, DefunctCoin, core.`None`).contains(_)))
      update(1)(core.`None`)(subtract)(player)
    else
      player
  }

  def checkForFoul = (player: Player) => {
    if (player.strike.count(List(StrikerStrike, DefunctCoin, core.`None`).contains(_)) == 3)
      update(1)(core.`None`)(subtract)(player)
    else
      player
  }


  def checkResult(players: List[Player]) =
    if (playersWithAtleastFivePoints(players).nonEmpty || playerWithAtleastThreePointsMore(players).isDefined) {
      val sortedPlayers = sortedPlayersByPoints(players)
      Winner(sortedPlayers.head.id, sortedPlayers.map(_.points))
    }
    else
      Draw

  def checkWinner(players: List[Player]) =
    (playersWithAtleastFivePoints andThen playerWithAtleastThreePointsMore) (players)

  private def playersWithAtleastFivePoints = (players: List[Player]) => players.filter(_.points >= 5)

  private def playerWithAtleastThreePointsMore = (players: List[Player]) => players.size match {
    case 0 => scala.None
    case 1 => Some(players.head)
    case _ => {
      val sorted = sortedPlayersByPoints(players)
      if (sorted.head.points - sorted(1).points >= 3) Some(sorted.head) else scala.None
    }
  }

  def sortedPlayersByPoints = (players: List[Player]) => players.sortWith(_.points > _.points)

  private def subtract(value1: Int, value2: Int) = value1 - value2

}