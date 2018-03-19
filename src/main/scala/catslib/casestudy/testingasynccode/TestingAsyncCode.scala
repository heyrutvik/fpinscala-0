package catslib.casestudy.testingasynccode

import scala.concurrent.Future
import cats._
import cats.implicits._

object TestingAsyncCode extends App {

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = ???
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}