package Chapter4Monads

import cats.data.State
import State._
import cats.implicits.catsSyntaxApplicativeId

object StateMonad extends App {
  val aState = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  println(aState)

  val (state, result) = aState.run(10).value // gets both state and result

  val justTheState = aState.runS(10).value // gets just the state

  val jusTheResult = aState.runA(10).value // gets just the result, ignore the state

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1 // gets the initial state via run
    b <- step2 // gets the state from previous step
  } yield (a, b)

  val (stateRes2, resultRes2) = both.run(20).value
  println(s"stateRes2 $stateRes2")
  println(s"resultRes2 $resultRes2")

  val getDemo = State.get[Int] // state becomes result
  println(getDemo.run(10).value)

  val setDemo = State.set[Int](30) // updates the state, result is unit
  println(setDemo.run(10).value)

  val pureDemo = State.pure[Int, String]("Result") // ignores the state, returns the supplied result
  println(pureDemo.run(10).value)

  val inspectDemo = State.inspect[Int, String](x => s"${x}!") // transforms the state via a transformation function
  println(inspectDemo.run(10).value)

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(10).value) // updates the state using an update function

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  // Exercise 4.9.3
  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  val v1 = evalOne("42").runA(Nil).value
  println(v1)

  val programOne = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  programOne.runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  val evalAllRes = multistageProgram.runA(Nil).value

  println(evalAllRes)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(biggerProgram.runA(Nil).value)
}
