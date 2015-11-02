package _3datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException(l.toString)
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException(l.toString)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // Can't be implemented in canstant time because our List implementation is singly linked
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def productFoldRight(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def reconstructList[A](l: List[A]) =
    foldRight(l, Nil: List[A])((a, acc) => Cons(a, acc))

  def length[A](l: List[A]) =
    foldRight(l, 0)((_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def productFoldLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def sumFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def lengthFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)((acc, _) => 1 + acc)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), Cons(h, Nil))
  }

  def reverseFoldLeft[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((b, a) => Cons(a, b))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => append(b, a))

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def toStrings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A], f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

}