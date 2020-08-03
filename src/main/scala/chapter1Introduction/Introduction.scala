package chapter1Introduction

object Introduction extends App {

  case class Person(name: String, email: String)

  // Define a very simple JSON AST
  sealed trait Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case object JsNull extends Json

  // The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] { // has at least one type parameter
    def write(value: A): Json
  }

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

    implicit val personWriter: JsonWriter[Person] = (value: Person) => JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        def write(option: Option[A]): Json =
          option match {
            case Some(aValue) => writer.write(aValue)
            case None => JsNull
          }
      }
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) { // this must have exactly one argument
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  import JsonWriterInstances._
  //  val personTc = JsonTypeClass.Json.toJson(Person("Dave", "dave@example.com"))

  import JsonSyntax._
  val personTc: Json = Person("Dave", "dave@example.com").toJson // extension method

  // println(personTc)

  // println(Option("A string").toJson)

  // Exercise 1: Printable

  // TC itself - trait with (at least) a type parameter
  trait Printable[A] {
    def format(a: A): String
  }

  // TC instances
  object PrintableInstances {
    implicit val printableString: Printable[String] = (str: String) => str

    implicit val printableInt: Printable[Int] = (int: Int) => int.toString
  }

  // generic interface methods
  // interface object?
  object Printable {
    implicit def format[A](value: A)(implicit printable: Printable[A]): String =
      printable.format(value)

    implicit def print[A](value: A)(implicit printable: Printable[A]): Unit =
      println(printable.format(value))
  }

  // better syntax - extension methods (previously known as type enrichment or pimping)
  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit printable: Printable[A]): String = printable.format(value)

      def print(implicit printable: Printable[A]): Unit = println(value.format)
    }
  }

  import Printable._
  import PrintableInstances._
  import PrintableSyntax._

  val formattedInt = format(3)

  // Exercise 2: Printing Cats
  case class Cat(name: String, age: Int, color: String)

  object Cat {
    import cats.Show

    implicit val printableCat: Printable[Cat] = (cat: Cat) =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."

    implicit val catShow: Show[Cat] = (cat: Cat) =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }

  val louie = Cat("Louie", 5, "ginger")

  louie.print

  // Meet Cats
  // step one - TC
  import cats.Show // cats version of the printable above
  // step two - instances
  import cats.instances.int._ // implicit instances of all cats TCs for int. means we can use Show[Int]
  import cats.instances.string._
  // step three - syntax
  import cats.syntax.show._ // for show

  val showInt = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  1223.show

  val stringAsString: String = showString.show("abc")
  "abC".show

  import java.util.Date

  implicit val dateShow: Show[Date] = new Show[Date] {
    def show(date: Date): String = s"${date.getTime}ms since the epoch"
  }

  println(new Date().show)

  println(louie.show)

  // Cats Eq
  import cats.Eq
  import cats.instances.int._
  import cats.syntax.eq._

  val eqInt = Eq[Int]

  // primitive syntax
  eqInt.eqv(123, 123)
  eqInt.eqv(123, 424)

  123 === 123
  123 =!= 542

  import cats.instances.option._
  import cats.syntax.option._
  // Some(1) === None  // breaks cause Some is not same tye as None => dirty fix below
  (Some(1): Option[Int]) === (None: Option[Int])
  // not so dirty
  Option(1) === Option.empty[Int]
  1.some === none[Int]
  1.some =!= none[Int]

  import cats.instances.long._ // for Eq

  implicit val dateEq: Eq[Date] = Eq.instance[Date] {
    (date1, date2) => date1.getTime === date2.getTime
  }

  val x = new Date()
  val y = new Date()
  x === x
  x === y

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {
    (kat1, kat2) =>
      kat1.name === kat2.name && kat1.age === kat2.age && kat1.color === kat2.color
  }

  println(cat1 === cat2)
  println(cat1 =!= cat2)

  optionCat1 === optionCat2
}
