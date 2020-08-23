package Chapter7TraversableAndFoldable

object TraversableIntro extends App {
  // Traverse is a higher-level abstraction that uses Applicatives to iterate with less pain than folding.

  import scala.concurrent._
  import scala.concurrent.duration._

  import scala.concurrent.ExecutionContext.Implicits.global
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  val allUptimes1: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime = getUptime(host)
        for {
          accum  <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }

  val allUptimes2: Future[List[Int]] = Future.traverse(hostnames)(getUptime)

  Await.result(allUptimes1, 1.second)
}
