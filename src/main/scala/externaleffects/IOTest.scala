package externaleffects

object IOTest extends App {
  def ReadLine: IO[String] = IO {
    scala.io.StdIn.readLine
  }

  def PrintLine(msg: String) = IO {
    println(msg)
  }

  def getValueFromUser = for {
    _ <- PrintLine("add 10 to this number: ")
    d <- ReadLine.map(_.toDouble + 10)
    _ <- PrintLine(d.toString)
  } yield ()

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
}