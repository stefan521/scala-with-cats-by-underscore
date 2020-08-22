package Chapter5MonadTransformers

import cats.implicits.catsSyntaxEitherId

object MonadTransformers extends App {
  import cats.data.OptionT
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  val separator = " <>" + "-" * 50 + "<> "

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  println(result1)
  println(separator)
  println(result2)

  val combiningMonads =
    result1.flatMap { (x: Int) =>
      result2.map { (y: Int) =>
        x + y
      }
    }

  println(separator)
  println(combiningMonads)

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.instances.either._

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y=> x+ y))

  import scala.concurrent.{Future, Await}
  import cats.data.{EitherT, OptionT}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import cats.implicits.catsStdInstancesForFuture

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  val intermediate: FutureEither[Option[Int]] = futureEitherOr.value
  val stack: Future[Either[String, Option[Int]]] = intermediate.value

  val bigStackResult: Either[String, Option[Int]] = Await.result(stack, 1.second)

  println(separator)
  println(bigStackResult)

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result: OptionT[Logged, Int] = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    val toReturn: Logged[Option[Int]] = result.value

    toReturn
  }

  val resultAddAll1 = addAll("1", "2", "3")
  val resultAddAll2 = addAll("1", "a", "3")

  println(separator)
  println(resultAddAll1)
  println(separator)
  println(resultAddAll2)

  // Exercise 5.4
  //  type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(powerLevel) => EitherT(Future(powerLevel.asRight))
    case None             => EitherT(Future(s"$autobot not found".asLeft))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      ally1Ready <- getPowerLevel(ally1)
      ally2Ready <- getPowerLevel(ally2)
    } yield (ally1Ready + ally2Ready) >= 15

  // equivalent without the for-comprehension
//  def canSpecialMoveRaw(ally1: String, ally2: String): Response[Boolean] =
//    getPowerLevel(ally1).flatMap(
//      ally1Ready => getPowerLevel(ally2)
//        .map(ally2Ready => (ally1Ready + ally2Ready) >= 15)
//    )

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"

      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"

      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  val tr1 = tacticalReport("Jazz", "Bumblebee")
  val tr2 = tacticalReport("Bumblebee", "Hot Rod")
  val tr3 = tacticalReport("Jazz", "Ironhide")

  println(tr1)
  println(separator)
  println(tr2)
  println(separator)
  println(tr3)
}
