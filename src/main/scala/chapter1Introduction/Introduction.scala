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

      def print(implicit printable: Printable[A]): Unit = println(printable.format(value))
    }
  }

  import Printable._
  import PrintableInstances._
  import PrintableSyntax._

  val formattedInt = format(3)

  // Exercise 2: Printing Cats
  case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val printableCat: Printable[Cat] = (cat: Cat) =>
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }

  val louie = Cat("Louie", 5, "ginger")

  louie.print
}
