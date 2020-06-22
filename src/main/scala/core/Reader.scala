package core

import scala.io.Source
import scala.util.{Failure, Success, Try}

trait Reader[A, B] {
  def read(input: A): B
}


object FileReader extends Reader[String, Either[Error, List[String]]]{

  override def read(filePath: String): Either[Error, List[String]] = {
    if(filePath.isEmpty) Left(FileNotFound)
    else{
      Try(Source.fromFile(filePath).getLines()) match {
        case Success(value) => Right(value.toList)
        case Failure(e) => Left(ErrorReadingFile(e.toString))
      }
    }
  }

}