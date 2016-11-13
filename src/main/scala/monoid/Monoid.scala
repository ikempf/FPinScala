package monoid

import monoid.MonoidInstances.endoMonoid

trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A

}

object Monoid {

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], monoid: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), monoid)

  def foldMapV[A, B](as: List[A], monoid: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), monoid)

  def foldRightMapSteps[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val partialApplications: List[(B) => B] = as.map(f.curried)
    val monoid: Monoid[(B) => B] = dual(endoMonoid[B])
    val composedFunction: (B) => B = partialApplications.foldLeft(monoid.zero)(monoid.op)
    composedFunction(z)
  }

  def foldRightMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(f.curried)(z)

  def foldLeftMapSteps[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val partialApplications: List[(B) => B] = as.map(a => (b: B) => f(b, a))
    val monoid: Monoid[(B) => B] = endoMonoid[B]
    val composedFunction: (B) => B = partialApplications.foldLeft(monoid.zero)(monoid.op)
    composedFunction(z)
  }

  def foldLeftMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)


}
