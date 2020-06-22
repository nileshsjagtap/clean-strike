package core

import domain.SequenceOP.sequence

trait Strike {

  def apply(strikes: List[String]) = sequence(strikes map getStrike, Nil)

  private def getStrike(strike: String) = strike match {
    case "Strike" => Right(Strike)
    case "Multi-strike" => Right(MultiStrike)
    case "Red strike" => Right(RedStrike)
    case "Striker strike" => Right(StrikerStrike)
    case "Defunct coin" => Right(DefunctCoin)
    case "White Strike" => Right(WhiteStrike)
    case "Multi-White-Strike" => Right(MultiWhiteStrike)
    case "None" => Right(`None`)
    case _ => Left(InvalidStrike)
  }

}

case object Strike extends Strike

case object MultiStrike extends Strike

case object RedStrike extends Strike

case object StrikerStrike extends Strike

case object DefunctCoin extends Strike

case object WhiteStrike extends Strike

case object MultiWhiteStrike extends Strike

case object `None` extends Strike


