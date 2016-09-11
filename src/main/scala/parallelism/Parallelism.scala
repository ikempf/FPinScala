package parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

class UnitFuture[A](a: A) extends Future[A] {
  override def isCancelled: Boolean = false
  override def get: A = a
  override def get(timeout: Long, unit: TimeUnit): A = get
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  override def isDone: Boolean = true
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    es => new UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def map2[A, B, C](l: Par[A], r: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val eventualA = l(es)
      val eventualB = r(es)
      new Future[C] {
        override def isCancelled: Boolean = false
        override def get: C = f(eventualA.get, eventualB.get)
        override def get(timeout: Long, unit: TimeUnit): C = {
          val start = System.nanoTime()
          val a = eventualA.get(timeout, unit)
          val b = eventualB.get(unit.toNanos(timeout - start), unit)
          f(a, b)
        }
        override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
        override def isDone: Boolean = true
      }
    }

  def fork[A](a: => Par[A]): Par[A] = es => {
    val value: Callable[A] = new Callable[A] {
      def call = a(es).get
    }

    es.submit(value)
  }

  def run[A](par: Par[A])(implicit es: ExecutorService): Future[A] =
    par(es)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

}