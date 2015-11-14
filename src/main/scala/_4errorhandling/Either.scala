package _4errorhandling

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(b) => Left(b)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(b) => Left(b)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2ViaForComprehension[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)


}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]):Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h::t => h.map2(sequence(t))(_::_)
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]) :Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_::_)
  }

  def traverseViaFoldRight[E, A, B](es: List[A])(f: A => Either[E, B]) :Either[E, List[B]] =
    es.foldRight(Right(Nil):Either[E, List[B]])((a, b) => f(a).map2(b)(_::_))

  def sequenceViaTraverse[E, A](es: List[Either[E, A]]):Either[E, List[A]] =
    traverse(es)(identity)

}