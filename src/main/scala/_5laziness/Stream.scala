package _5laziness

import _5laziness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

trait Stream[+A] {

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

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
    foldRight(empty[A])((a, z) => if (f(a)) cons(a, z) else Empty)

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, z) => cons(f(a), z))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, z) => if (f(a)) cons(a, z) else z)

  def append[B >: A](stream2: Stream[B]): Stream[B] =
    foldRight(stream2)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, z) => f(a).append(z))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), m) if m > 0 => Some(h(), (t(), m - 1))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, stream)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B, C](stream: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, stream)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def startsWith[B >: A](stream: Stream[B]): Boolean =
    zipAll(stream).takeWhile(_._2.isDefined).forAll(h => h._1 == h._2)

  def tails: Stream[Stream[A]] = this match {
    case Cons(_, t) => cons(this, t().tails)
    case Empty => Stream(Empty)
  }

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // !TODO: get this to work ...
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    def scanInner(s: Stream[A], z2:B, y: Stream[B]): Stream[B] = s match {
      case Empty => y
      case Cons(h, t) => scanInner(t(), f(h(), z2), cons(f(h(), z2), y))
    }

    scanInner(this, z, Stream(z))
  }

  def scanRightViaFoldRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      val b = f(a, acc._1)
      (b, cons(b, acc._2))
    })._2
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fib: Stream[Int] = {
    def fibInner(f1: Int, f2: Int): Stream[Int] = cons(f1, fibInner(f2, f1 + f2))

    fibInner(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromViaUnfold(a: Int): Stream[Int] = unfold(a)(s => Some(s, s + 1))

  def fibViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f1, f2) => Some(f1, (f2, f1 + f2)) }

  def asList[A](stream: Stream[Stream[A]]): List[List[A]] =
    stream.map((a) => a.toList).toList

}