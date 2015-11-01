package _2gettingstarted

import _2gettingstarted.GettingStarted.{fibTailRecursive, fib, isSorted}
import org.scalatest.{FlatSpec, Matchers}

class GettingStartedTest extends FlatSpec with Matchers with FibonacciBehavior {
  val IntNaturalOrder: (Int, Int) => Boolean = (a: Int, b: Int) => a < b
  val IntInverseOrder: (Int, Int) => Boolean = (a: Int, b: Int) => b < a

  "Naive Fibonacci" should behave like fibonacciBehaviour(fib)
  "Tail recursive Fibonacci" should behave like fibonacciBehaviour(fibTailRecursive)

  "IsSorted" should "return true for ordered arrays" in {
    isSorted(Array(1, 2, 3, 4), IntNaturalOrder)
    isSorted(Array(1, 13, 25, 187), IntNaturalOrder)
    isSorted(Array(-899, -10, 0, 1, 14), IntNaturalOrder)

    isSorted(Array(14, 1, -10), IntInverseOrder)

    isSorted(Array("a", "aa", "cdg"), (a:String, b:String) => a.length < b.length)
  }

  it should "return false for unordered arrays" in {
    isSorted(Array(1, 2, 0), IntNaturalOrder)
    isSorted(Array(-5, -2, -3), IntNaturalOrder)
    isSorted(Array(-5, -2, 0, 4), IntNaturalOrder)

    isSorted(Array(4, 0, -2, -5), IntInverseOrder)
  }

}


