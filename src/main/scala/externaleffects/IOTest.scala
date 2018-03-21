package externaleffects

object IOTest extends App {
  def readLine: IO[String] = IO {
    scala.io.StdIn.readLine
  }

  def printLine(msg: String) = IO {
    println(msg)
  }

  def getValueFromUser = for {
    _ <- printLine("Enter a temperature in degrees fahrenheit: ")
    d <- readLine.map(_.toDouble + 10)
    _ <- printLine(d.toString)
  } yield ()
}