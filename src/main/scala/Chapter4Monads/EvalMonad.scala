package Chapter4Monads

import cats.Eval

object EvalMonad extends App {
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
}
