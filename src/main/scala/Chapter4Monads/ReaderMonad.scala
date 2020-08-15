package Chapter4Monads

import cats.data.Reader

// important thing about readers is they provide a tool for dependency injection (they are also composable since they're monads and that's nice)
object ReaderMonad extends App {
  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  val readerRes1 = catName.run(Cat("Garfield", "lasagne"))
  println(readerRes1)

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

  val readerRes2 = greetKitty.run(Cat("Heathcliff", "junk food"))
  println(readerRes2)

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty // the flatMap runs the Reader it seems
      feed <- feedKitty
    } yield s"$greet. $feed"

  val greetFeedGarfield = greetAndFeed(Cat("Garfield", "lasagne")) // EXACTLY THE SAME AS greetAndFeed.run(Cat("Garfield", "lasagne"))
  println(greetFeedGarfield)

  final case class Db(
                       usernames: Map[Int, String],
                       passwords: Map[String, String]
                     )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String
                   ): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  val dbReaderFalse: DbReader[Boolean] = Reader(_ => false) // for pure

  def checkLogin(
                  userId: Int,
                  password: String
                ): DbReader[Boolean] =
    for {
      username
        <- findUsername(userId)
      passwordOk <- username match {
        case Some(uname) => checkPassword(uname, password)
        case _ => dbReaderFalse
      }
    } yield passwordOk

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  val login1 = checkLogin(1, "zerocool").run(db)
  println(s" LOGIN 1?? $login1")

  val login2 = checkLogin(2, "ratatatatata")(db)
  println(s" LOGIN 2?? $login2")

  val login3 = checkLogin(109421, "noHacksPromise").run(db)
  println(s"LOGIN 3?? $login3")
}
