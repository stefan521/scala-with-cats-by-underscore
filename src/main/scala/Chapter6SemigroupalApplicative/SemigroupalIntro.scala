package Chapter6SemigroupalApplicative

import cats.syntax.either._ // for catchOnly

object SemigroupalIntro extends App {
  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => s"Couldn't read $str")

  val res1 =
    for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield a + b + c

  val separator = "-" * 50

  println(res1)
  println(separator)

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  val res2 = Semigroupal[Option].product(Some(123), Some("abc"))
  println(res2)
  println(separator)

  val res3 = Semigroupal[Option].product(None, Some("abc"))
  println(res3)
  println(separator)

  val res4 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  println(res4)
  println(separator)

  val res5 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  println(res5)
  println(separator)

  import cats.syntax.apply._
  val res6 = (Option(123), Option("abc")).tupled
  println(res6)
  println(separator)

  case class Cat(name: String, born: Int, color: String)

  val res7 = (
    Option("Garfield"),
    Option(1978),
    Option("Orange and black")
  ).mapN(Cat.apply) // mapN uses semigroupal to extract the values from option and functor to apply the values to the function

  println(res7)
  println(separator)

  import cats.Semigroupal
  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  // semigroupal allows concurrent execution, rather than sequential
  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

  Await.result(futurePair, 1.second)

  // monads are also semigroupals and applicatives. semigroupals are interesting when they are not also monads
  // fancier way to say the above is "for monads, the semantics of product are equivalent to the semantics of flatMap"
}
