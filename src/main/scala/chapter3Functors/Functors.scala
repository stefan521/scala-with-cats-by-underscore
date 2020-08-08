package chapter3Functors

import cats.Functor

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.instances.function._
import cats.syntax.functor._


object Functors extends App {
  val future: Future[String] =
    Future(123)
      .map(n => n + 1)
      .map(n => n * 2)
      .map(n => s"$n!!!")

  Await.result(future, 1.second)
  // res2: String = "248!"

  future.onComplete(println)

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  // function composition using map
  (func1 map func2)(1)

  // function composition using andThen
  (func1 andThen func2)(1)

  // composition written out by hand
  func2(func1(1))

  val func =
    ((x: Int) => x.toDouble)
      .map(x => x + 1)
      .map(x => x * 2)
      .map(x => s"$x!")

  func(123)

  // Exercise 3.5.4
  sealed trait Tree[+A]
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  Tree.leaf(100).map(_ * 2)
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  // Exercise 3.6.1.1 contramap
  trait Printable[A] {
    self => // same as private[this] val self = this

    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
  }

  implicit val stringPrintable: Printable[String] = (value: String) =>
    "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) =>
    if(value) "yes" else "no"

  println(format(true))
  println(format("hello"))

  final case class Box[A](value: A)

  implicit def boxPrintable[A]: Printable[Box[A]] =
    stringPrintable.contramap(box => s"Box[${box.value}]")

  println(format(Box("hello world")))
  println(format(Box(true)))

  trait Codec[A] {
    self =>

    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  println(encode(2.3))

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap(Box(_), _.value)

  println(encode(Box(123.4)))
}
