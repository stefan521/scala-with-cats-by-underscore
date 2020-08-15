package Chapter4Monads

import cats.Monad
import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent._
import scala.concurrent.duration._

object MonadsPlayground extends App {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if(b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] = {
//    parseInt(aStr).flatMap { aNum =>
//      parseInt(bStr).flatMap { bNum =>
//        divide(aNum, bNum)
//      }
//    }

    // same as
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      answer <- divide(aNum, bNum)
    } yield answer
  }

  stringDivideBy("6", "2")

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  // Exercise 4.1.2
  trait ShedMadeMonad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(resA => pure(func(resA)))
  }

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)

  val list1 = Monad[List].pure(3)
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  val list3 = Monad[List].map(list2)(a => a + 123)

  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]

  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

  Await.result(future, 1.second)

  1.pure[Option]

  1.pure[List]

  sumSquare(Option(3), Option(4))

  sumSquare(List(1, 2, 3), List(4, 5))

  import cats.Id
  sumSquare(3: Id[Int], 4: Id[Int])

  // Exercise 4.3.1
  def pure[A](value: A): Id[A] = value

  def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)

  def flatMap[A, B](value: A)(f: A => Id[B]): Id[B] = f(value)

  val a = 3.asRight[String]
  val b = 4.asRight[String]

  for {
    x <- a
    y <- b
  } yield x*x + y*y

  /*
   below is a bug caused by over-narrowing types
   to fix it replace Right(0) with 0.asRight (use smart constructor that returns an Either not a Right)
   */
//  def countPositive(nums: List[Int]) =
//    nums.foldLeft(Right(0)) { (accumulator, num) =>
//      if(num > 0) {
//        accumulator.map(_ + 1)
//      } else {
//        Left("Negative. Stopping!")
//      }
//    }
}
