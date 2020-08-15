package Chapter4Monads

import cats.Id
import cats.data.Writer
import cats.syntax.writer._ // for tell
import cats.syntax.applicative._ // for written
import cats.implicits.catsKernelStdMonoidForVector // for combining logs

// for futures
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Await, Future}

object WriterMonad extends App {
  type Logged[A] = Writer[Vector[String], A]

  Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)

  Vector("msg1", "msg2", "msg3").tell

  val aWriter = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val bWriter = 123.writer(Vector("msg1", "msg2", "msg3"))

  val anIntFromWriter: Int = aWriter.value
  val justLog: Id[Vector[String]] = aWriter.written

  val (aLog: Seq[String], anotherInt: Int) = bWriter.run

  // got to 4.7.2
  val writer1 = for {
    a <- 10.pure[Logged] // log is empty and 10 is the result
    _ <- Vector("a", "b", "c").tell // log is a, b, c and no result
    b <- 32.writer(Vector("x", "y", "z")) // log is x, y, z and result 32
  } yield a + b // combines results? logs are appended. that's why vector is better cause faster append

  println(s"Writer 1 log: ${writer1.written}")

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  println(s"Writer 2 : ${writer2.run}")

  // Writers have a log and a result. biMap an mapBoth can modify the log and result at the same time
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  println(s"Writer3: ${writer3.run}")

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  println(s"Writer4: ${writer4.run}")

  val writer5 = writer1.reset

  println(s"Writer5: ${writer5.run}")

  val writer6 = writer1.swap

  println(s"Writer 6: ${writer6.run}")

  // Exercise 4.7.3
  object Exercise473 {
    def slowly[A](body: => A): A =
      try body finally Thread.sleep(100)

    def factorial(n: Int, logPrefix: String = ""): Logged[Int] = {
      val factResult =
        if (n == 0) 1.pure[Logged]
        else slowly(factorial(n - 1, logPrefix).map(_ * n))

      for {
        ans <- factResult
        _ <- Vector(s"$logPrefix fact $n $ans").tell
      } yield ans
    }

    def startExercise =
      Await.result(Future.sequence(Vector(
        Future(factorial(3, "[F1]")),
        Future(factorial(3, "[F2]"))
      )), 5.seconds)
  }

  Exercise473.startExercise
    .map(_.written)
    .foreach(println(_))
}
