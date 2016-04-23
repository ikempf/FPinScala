package gettingstarted

import scala.annotation.tailrec

object GettingStarted {

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case m if m > 0 => fib(n - 1) + fib(n - 2)
    case _ => throw new IllegalArgumentException(n.toString)
  }

  def fibTailRecursive(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException(n.toString)

    @tailrec
    def loop(i: Int, a: Int, b: Int): Int = i match {
      case _ if i >= n => a
      case _ => loop(i + 1, b, a + b)
    }

    loop(0, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = n match {
      case _ if n >= as.length => true
      case _ if !ordered(as(n - 1), as(n)) => false
      case _ => loop(n + 1)
    }

    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B) =
    (a: A) => f(g(a))

}
