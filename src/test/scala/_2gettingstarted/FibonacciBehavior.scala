package _2gettingstarted

import org.scalatest.{FlatSpec, Matchers}


trait FibonacciBehavior extends Matchers {
  this: FlatSpec =>

  def fibonacciBehaviour(fib: Int => Int) {
    it should "process the nth number" in {
      fib(0) should equal(0)
      fib(1) should equal(1)
      fib(2) should equal(1)
      fib(3) should equal(2)
      fib(4) should equal(3)
      fib(5) should equal(5)
      fib(5) should equal(5)
      fib(25) should equal(75025)
    }

    it should "throw IllegalArgumentException when given a negative input" in {
      intercept[IllegalArgumentException] {
        fib(-1)
      }
    }
  }
}