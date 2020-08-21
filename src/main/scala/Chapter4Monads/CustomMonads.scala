package Chapter4Monads

import cats.Monad
import scala.annotation.tailrec

object CustomMonads extends App {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    @tailrec
    override def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }

    override def pure[A](opt: A): Option[A] = Some(opt)
  }

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    // flatmapping trees allows them to fan out cause each node can become a whole new tree
    override def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] =
      tree match {
        case Leaf(value) => fn(value)
        case Branch(left, right) => branch(flatMap(left)(fn), flatMap(right)(fn))
      }

    // not stack safe - can blow up. all monads in cats are stack safe
    override def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(fn(a)) {
        case Left(value) => tailRecM(value)(fn)
        case Right(value) => Leaf(value)
      }

    override def pure[A](a: A): Tree[A] = leaf(a)
  }

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  val res1 = branch(leaf(100), leaf(200))
    .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  val res2 =
    for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

  println(res1)
  println(" <>" + "-" * 50 + "<> ")
  println(res2)
}
