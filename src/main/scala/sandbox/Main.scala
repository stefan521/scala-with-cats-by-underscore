package sandbox

import cats.instances.string._
import cats.syntax.semigroup._

case class Person(name: String, email: String)

object JsonTypeClass {
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
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }
}

object Main extends App {
  println("Hello " |+| "Cats!")

  import JsonTypeClass.JsonWriterInstances._
//  val personTc = JsonTypeClass.Json.toJson(Person("Dave", "dave@example.com"))

  import JsonTypeClass.JsonSyntax._
  val personTc = Person("Dave", "dave@example.com").toJson // extension method

  println(personTc)
}
