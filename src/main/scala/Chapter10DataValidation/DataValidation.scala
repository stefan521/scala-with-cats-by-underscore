package Chapter10DataValidation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._


object DataValidation extends App {

  sealed trait Check[E, A] {
    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) => (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Check {
    final case class And[E, A](
      left: Check[E, A],
      right: Check[E, A]
    ) extends Check[E, A]

    final case class Or[E, A](
      left: Check[E, A],
      right: Check[E, A]
    ) extends Check[E, A]

    final case class Pure[E, A](
      func: A => Validated[E, A]
    ) extends Check[E, A]
  }

  val aCondition = Check.Pure[Vector[String], Int] { v =>
    if(v > 2) v.valid
    else Vector("Must be > 2").invalid
  }

  val bCondition: Check[Vector[String], Int] = Check.Pure { v =>
    if(v < -2) v.valid
    else Vector("Must be < -2").invalid
  }

  val impossibleCheck = Check.And(aCondition, bCondition)

  val res1 = impossibleCheck(5)
  val res2 = impossibleCheck(1)

  println(res1)
  println(res2)
}
