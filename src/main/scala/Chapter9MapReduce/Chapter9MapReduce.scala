package Chapter9MapReduce

import cats.Monoid

import cats.instances.int._
import cats.instances.string._
import cats.instances.future._
import cats.instances.vector._

import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.semigroup._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Chapter9MapReduce extends App {

  // Exercise 9.2
  def foldMap[A, B: Monoid](vec: Vector[A])(fn: A => B): B = {
    vec.foldLeft(Monoid[B].empty)(_ |+| fn(_))
  }

  val res1 = foldMap(Vector(1, 2, 3))(identity)
  val res2 = foldMap(Vector(1, 2, 3))(_.toString + "! ")
  println(res1)
  println(res2)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(fn: A => B): Future[B] = {
    val numberOfCores = Runtime.getRuntime.availableProcessors
    val batchSize = (1.0 * values.size / numberOfCores).ceil.toInt
    val batches = values
      .grouped(batchSize)
      .map(batch => Future(foldMap(batch)(fn)))

    Future.sequence(batches).map(iter => iter.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  val res3 = parallelFoldMap(
    Vector.fill(30)(30)
  )(identity)

  res3.onComplete(res => println(res))

  def parallelFoldMapWithCats[A, B: Monoid](values: Vector[A])(fn: A => B): Future[B] = {
    val numberOfCores = Runtime.getRuntime.availableProcessors
    val batchSize = (1.0 * values.size / numberOfCores).ceil.toInt

    values
      .grouped(batchSize)
      .toVector
      .traverse(group => Future(group.foldMap(fn)))
      .map(_.combineAll)
  }
}
