package domain

import core.{Error, InvalidStrike, Strike}

object SequenceOP {

  def sequence(list: List[Either[Error, Strike]], acc: List[Strike]): Either[Error, List[Strike]] = list match {
    case Nil => Right(acc)
    case h :: _ if (h.isLeft) => Left(InvalidStrike)
    case h :: t => sequence(t, acc.:+(h.right.get))
  }

}
