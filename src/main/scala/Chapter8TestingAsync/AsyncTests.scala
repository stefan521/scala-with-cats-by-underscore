package Chapter8TestingAsync

import cats.{Applicative, Id}

import scala.concurrent.Future
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._


object AsyncTests extends App {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  class UptimeService[F[_]: Applicative](uptimeClient: UptimeClient[F]) {
    def getTotalUptime(hosts: List[String]): F[Int] =
      hosts.traverse(uptimeClient.getUptime).map(_.sum)
  }

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum

    assert(actual == expected)
  }
}
