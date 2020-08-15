package Chapter4Monads

import cats.implicits._

object EitherMonad extends App {
  Either.catchOnly[NumberFormatException]("foo".toInt)
  Either.catchNonFatal(sys.error("Badness"))
  Either.fromTry(scala.util.Try("foo".toInt))
  Either.fromOption[String, Int](None, "Badness")

  "Error".asLeft[Int].getOrElse(0)
  "Error".asLeft[Int].orElse(2.asRight[String])

  (-1).asRight[String].ensure("Must be non-negative!")(_ > 0)

  "error".asLeft[Int].recover {
    case _: String => -1
  }

  "error".asLeft[Int].recoverWith {
    case _: String => Right(-1)
  }

  "foo".asLeft[Int].leftMap(_.reverse)

  6.asRight[String].bimap(_.reverse, _ * 7)

  123.asRight[String] // this string type means the left type cause the right type can already be inferred

  val infinity = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if(b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100

  println(infinity)

  // Note: to represent errors you could use throwables (and go back to Java) or define an ADT
  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String) extends LoginError

  final case class PasswordIncorrect(username: String) extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(username) =>
        println(s"User not found: $username")

      case PasswordIncorrect(username) =>
        println(s"Password incorrect: $username")

      case UnexpectedError =>
        println("Unexpected Error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft

  result1.fold(handleError, println)
  result2.fold(handleError, println)
}
