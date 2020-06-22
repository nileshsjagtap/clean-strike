package core

trait Error

case object InvalidBlackCoinCount extends Error

case object InvalidRedCoinCount extends Error

case object InvalidWhiteCoinCount extends Error

case object InvalidPlayerIds extends Error

case object InvalidTurn extends Error

case object InvalidStrike extends Error

case object InvalidPlayersAndStrikes extends Error

case object FileNotFound extends Error

case class ErrorReadingFile(err: String) extends Error

case class ErrorWritingFile(err: String) extends Error