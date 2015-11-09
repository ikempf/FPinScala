package _4errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.foldRight(0: Double)(_ + _) / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .map((m) => xs.foldRight(0d)((y, z) => math.pow(y - m, 2) + z))
      .map(_ / xs.length)
  }

  def variance2(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .flatMap((m) => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a:Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a2), Some(b2)) => Some(f(a2, b2))
    }
  }

//  def sequence[A](xs:List[Option[A]]): Option[List[A]] =
//    xs.foldRight(Some(Nil):Option[A])()
}