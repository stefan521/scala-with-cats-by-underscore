package Chapter4Monads

import Chapter4Monads.Playground.sumSquare
import cats.Monad
import cats.instances.option._
import cats.instances.list._
import cats.instances.future._

import scala.concurrent._
import scala.concurrent.duration._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.either._

object Playground {
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y
}

object Monads extends App {
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

  // Exercise 4.1.2
  trait Monad[F[_]] {
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

  // cats.Eval
  import cats.Eval

  val now = Eval.now(math.random + 1000)
  val later = Eval.later(math.random + 2000)
  val always = Eval.always(math.random + 3000)

  now.value // eager memorized evaluation
  later.value // lazy memorized evaluation
  always.value // lazy not memorized evaluation

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }

  ans.value
  ans.value

  // can stack overflow (not the website)
  //  def factorial(n: BigInt): BigInt =
  //    if(n == 1) n else n * factorial(n - 1)

  // stack safe
  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1)
      Eval.now(n)
    else
      Eval.defer {
        factorial(n - 1).map(_ * n)
      }

  factorial(50000).value

  // Exercise 4.6.4
  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _)) // here it is important not to call value cause that counts as access and forces evaluation
    }.value

  println(foldRight((1 to 50000).toList, 0L)(_ + _))

  // Writer monad
  import cats.data.Writer
  import cats.syntax.writer._ // for tell

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
}
