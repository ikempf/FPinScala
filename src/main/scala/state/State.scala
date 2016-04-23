package state

import scala.Int.{MaxValue, MinValue}


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object state {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt

    val res = v match {
      case MinValue => MaxValue
      case x if x < 0 => -x
      case x => x
    }

    (res, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)

    (v / (MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val res = intDouble(rng)
    (res._1.swap, res._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil: List[Int], rng))((z, a) => {
      val (i, r) = nonNegativeInt(z._2)
      (i :: z._1, r)
    })
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap =
    map(nonNegativeInt)(_ / (MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft((Nil: List[A], rng))((z, ra) => {
        val (a, rng) = ra(z._2)
        (a :: z._1, rng)
      })
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](rand: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = rand(rng)
      f(a)(r)
    }

  def mapViaFlatMap[A, B](rand: Rand[A])(f: A => B): Rand[B] =
    flatMap(rand)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B) =
      State((s: S) => {
        val (a, s2) = run(s)
        (f(a), s2)
      })

    def flatMap[B](f: A => State[S, B]) = {
      State((s: S) => {
        val (a, s2) = run(s)
        f(a).run(s2)
      })
    }


  }

  object State {

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def map2[S, A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C) =
      State((s: S) => {
        val (a, sa) = s1.run(s)
        val (b, sb) = s2.run(sa)
        (f(a, b), sb)
      })

  }

}
