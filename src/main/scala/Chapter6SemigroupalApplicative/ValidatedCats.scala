package Chapter6SemigroupalApplicative

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._

import scala.util.Try // for Monoid

object ValidatedCats extends App {
  type AllErrorsOr[A] = Validated[List[String], A]

  val res1 = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  println(res1)

  // Validated has Validated.Valid and Validated Invalid that are like Right and Left
  Validated.Valid(123)
  Validated.Invalid(List("Badness"))
  Validated.valid[List[String], Int](123) // smart constructors widen the type
  Validated.invalid[List[String], Int](List("Badness"))

  import cats.syntax.validated._ // for valid and invalid
  123.valid[List[String]]
  List("Badness").invalid[Int]

  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError

  type ErrorsOr[A] = Validated[List[String], A]
  123.pure[ErrorsOr]
  List("Badness").raiseError[ErrorsOr, Int]

  Validated.catchOnly[NumberFormatException]("foo".toInt)
  Validated.catchNonFatal(sys.error("Badness"))
  Validated.fromTry(scala.util.Try("foo".toInt))
  Validated.fromEither[String, Int](Left("Badness"))
  Validated.fromOption[String, Int](None, "Badness")

  import cats.instances.string._ // for Semigroup
  import cats.syntax.apply._ // for tupled
  import cats.instances.vector._ // for Semigroupal

  Semigroupal[AllErrorsOr]

  (
    "Error 1".invalid[Int],
    "Error 2".invalid[Int]
  ).tupled

  (
    Vector(404).invalid[Int],
    Vector(500).invalid[Int]
  ).tupled


  import cats.data.NonEmptyVector

  (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  ).tupled

  123.valid.map(_ * 100)

  "?".invalid.leftMap(_.toString)

  123.valid[String].bimap(_ + "!", _ * 100)

  "?".invalid[Int].bimap(_ + "!", _ * 100)

  // we CANNOT flatMap because Validated is not a monad. Cats provides andThen which usually can be used instead
  32.valid.andThen { a =>
    10.valid.map { b =>
      a + b
    }
  }

  import cats.syntax.either._

  "Badness".invalid[Int]
  "Badness".invalid[Int].toEither
  "Badness".invalid[Int].toEither.toValidated

  "fail".invalid[Int].getOrElse(0)
  "fail".invalid[Int].fold(_ + "!!!", _.toString)

  // Exercise 6.4.4 Form Validation
  type HtmlData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  // name and age must be specified
  // name must not be blank
  // age must be a valid non-negative integer
  case class User(name: String, age: Int)

  def getValue(field: String)(data: HtmlData): FailFast[String] =
    data.get(field).toRight(List(s"$field field not specified"))

  val nameDb = Map("name" -> "Dade Murphy")
  val getName = getValue("name") _
  getName(nameDb)
  getName(Map())

  def parseInt(name: String)(s: String): FailFast[Int] =
    Either.fromTry(Try(s.toInt)).leftMap(_ => List(s"$name is not convertible to Int"))

  parseInt("age")("11")
  parseInt("age")("foo")

  def isNonBlank(name: String)(s: String): FailFast[String] =
    Right(s).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def isNotNegative(name: String)(num: Int): FailFast[Int] =
    Right(num).ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(data: HtmlData): Either[List[String], String] = {
    val nameField = "name"

    for {
      name <- getValue(nameField)(data)
      _ <- isNonBlank(nameField)(name)
    } yield name
  }

  val res2 = readName(nameDb)
  println(res2)
  val res3 = readName(Map())
  println(res3)
  val res4 = readName(Map("name" -> ""))
  println(res4)

  def readAge(data: HtmlData): Either[List[String], Int] = {
    val ageField = "age"

    for {
      age <- getValue(ageField)(data)
      _ <- isNonBlank(ageField)(age)
      ageInt <- parseInt(ageField)(age)
      _ <- isNotNegative(ageField)(ageInt)
    } yield ageInt
  }

  def readUser(data: HtmlData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)
}
