package domain

import Rules._
import Player._
import CarromBoard._
import core.{Result, Strike, Winner}

case class Game(turns: List[Turn], players: List[Player], carromBoard: CarromBoard)

object Game {

  def start(game: Game, turns: List[Turn]): Result = turns match {
    case Nil => checkResult(game.players)
    case _ :: _ if checkWinner(game.players).isDefined => Winner(checkWinner(game.players).get.id, sortedPlayersByPoints(game.players).map(_.points))
    case h :: t => if (hasCoin(game.carromBoard, h.strike)) start(updateGame(game, h), t) else start(game, t)
  }

  private def updateGame(game: Game, turn: Turn) =
    game.copy(players = game.players.updated(turn.playerIdx, updatePlayerAndCheckRules(game.players(turn.playerIdx), turn.strike)),
      carromBoard = CarromBoard.updateCarromBoard(turn.strike)(game.carromBoard))

  private def updatePlayerAndCheckRules = (player: Player, strike: Strike) => (updatePlayer(strike) andThen checkRules) (player)

}
