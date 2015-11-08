package _3datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(n) => f(n)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maxViaFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ + _)

  def foldWithAcc[A, B](t: Tree[A], z: B)(f: (A, B) => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(foldWithAcc(l, z)(f)(g), foldWithAcc(r, z)(f)(g))
    case Leaf(a) => f(a, z)
  }

}