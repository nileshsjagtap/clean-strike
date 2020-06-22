package core

trait Result

case class Winner(winnerId: Int, finalScore: List[Int]) extends Result

case object Draw extends Result