package TraversableAndFoldable

import cats.Foldable


object FoldableIntro extends App {
  // Foldable abstracts the familiar foldLeft and foldRight opera ons;

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  show(Nil)

  show(List(1, 2, 3))

  // Exercise 7.1.2
  val res1 = List(1, 2, 3).foldLeft(List.empty[Int]) {
    (acc: List[Int], num: Int) =>  num :: acc
  }

  val res2 = List(1, 2, 3).foldRight(List.empty[Int]) {
    (num: Int, acc: List[Int]) =>  num :: acc
  }

  println(res1)
  println(res2)

  // Exercise 7.1.3
  def mapList[A, B](list: List[A])(fn: A => B): List[B] =
    list.foldRight(List.empty[B]) {
      (a: A, lstB: List[B]) => fn(a) :: lstB
    }

  val res3 = mapList(List(1, 2, 3))(_ * 10)
  println(res3)

  def flatMapList[A, B](list: List[A])(fn: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) {
      (a: A, lstB: List[B]) => fn(a) ::: lstB
    }

  val res4 = flatMapList[Int, Int](List(1, 2, 3))(num => List(num, num * 100))
  println(res4)

  def filterList[A](list: List[A])(fn: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) {
      (a: A, lst: List[A]) =>
        if (fn(a)) a :: lst
        else lst
    }

  val res5 = filterList(List(55, 10, 241, 501, 22, 515, 50))(_ > 100)
  println(res5)

  def sumList[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  val res6 = sumList(List(51, 120, 30))
  println(res6)

  import cats.instances.string._
  import cats.instances.list._

  val res7 = Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  println(res7)

  import cats.instances.vector._ // for Monoid
  import cats.instances.int._

  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))

  val res8 = (Foldable[List] compose Foldable[Vector]).combineAll(ints)
  println(res8)

  import cats.syntax.foldable._

  val res9 = List(1, 2, 3).combineAll
  println(res9)

  val res10 = List(1, 2, 3).foldMap(_.toString)
  println(res10)
}
