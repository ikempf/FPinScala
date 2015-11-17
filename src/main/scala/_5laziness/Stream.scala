package _5laziness

import _5laziness.Stream.cons

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListTailRecursive: List[A] = {
    @tailrec
    def loop(acc: List[A], stream: Stream[A]): List[A] = stream match {
      case Cons(h, t) => loop(h() :: acc, t())
      case _ => acc
    }

    loop(List(), this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Empty
  }

  @tailrec
  final def forAll(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) && t().forAll(f)
    case Empty => true
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, z) => if (f(a)) cons(a, z) else Empty)

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}