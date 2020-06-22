package app

import core._
import domain._

object Run extends App {

  def run   = {
    for {
      fileInput <- FileReader.read("target/resources/inputFile")
      carromBoard <- CarromBoard(9, 1, 6)
      players <- Player(List(11, 21))
      strikes <- Strike(fileInput)
      turns <- Turn(players, strikes)
    } yield Game(turns, players, carromBoard)
  }

  run.fold(err => writeErrorToFIle(err), game => writeResultToFile(Game.start(game, game.turns)))

  def writeResultToFile(result: Result) = result match {
      case Winner(id, pointsList) => FileWriter.write(s"Player with ID ${id} won the game. Final score: ${pointsList}", "target/resources/outputFile")
      case Draw => FileWriter.write(s"DRAW", "target/resources/outputFile")
  }

  def writeErrorToFIle(err:Error) = FileWriter.write(err.toString, "target/resources/outputFile")
}
