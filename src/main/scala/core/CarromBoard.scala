package core

import domain.Player

trait StrikeUpdate[Strike] {
  def update(player: Player) : Player
}

object StrikeUpdat {

  implicit object updatePlayerForStrike extends StrikeUpdate[Strike.type] {
    override def update(player: Player): Player = player.copy(points = player.points + 1, strike = player.strike.:+(Strike))
  }

  implicit object updatePlayerForMultiStrike extends StrikeUpdate[MultiStrike.type] {
    override def update(player: Player): Player = player.copy(points = player.points + 2, strike = player.strike.:+(MultiStrike))
  }

  implicit object updatePlayerForRedStrike extends StrikeUpdate[RedStrike.type] {
    override def update(player: Player): Player = player.copy(points = player.points + 3, strike = player.strike.:+(RedStrike))
  }



}

object StrikeUpdateUtil {
  import StrikeUpdat._
  def apply[A](strike: A, player: Player)(implicit as : StrikeUpdate[A]) = as.update(player)
}
