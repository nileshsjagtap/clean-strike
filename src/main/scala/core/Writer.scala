package core

import java.io.PrintWriter
import scala.util.{Failure, Success, Try}

trait Writer[A, B, C] {
  def write(value: A, source: B): C
}

object FileWriter extends Writer[String, String, Either[Error, String]] {

  override def write(value: String, source: String) = {
    val writer = new PrintWriter(source)
    Try {
      writer.write(value)
    } match {
      case Success(_) => writer.close(); Right("GameFinished")
      case Failure(exception) => writer.close(); Left(ErrorWritingFile(exception.toString))
    }
  }

}
