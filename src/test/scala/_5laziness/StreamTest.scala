package _5laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  "ToList" should "convert stream to list" in {
    Stream().toList should equal(Nil)
    Stream(1, 5, 9).toList should equal(List(1, 5, 9))
  }

  "ToListTailRecursive" should "convert stream to list" in {
    Stream().toListTailRecursive should equal(Nil)
    Stream(1, 5, 9).toListTailRecursive should equal(List(1, 5, 9))
  }

  "Take" should "take n first elemnts of the stream" in {
    Stream(1).take(2).toList should equal(List(1))
    Stream(1, 2, 3).take(2).toList should equal(List(1, 2))
  }

  "Drop" should "drop the n first elements" in {
    Stream(1, 2, 5, 7).drop(2).toList should equal(List(5, 7))
    Stream(1, 2).drop(2).toList should equal(List())
    Stream().drop(2).toList should equal(List())
  }

  "TakeWhile" should "take starting elements respecting the given predicate" in {
    Stream(5, 6, 7, 8).takeWhile(_ <= 7).toList should equal(List(5, 6, 7))
    Stream(5, 6, 7, 8).takeWhile(_ < 3).toList should equal(List())
  }

  "ForAll" should "check if all elements satisfy the given predicate" in {
    Stream(1, 2, 3, 4).forAll(_ < 5) should be(true)
    Stream(1, 2, 3, 4).forAll(_ % 2 == 0) should be(false)
    Stream().forAll(_ => false) should be(true)
  }

  "TakeWhileViaFoldRight" should "take starting elements respecting the given predicate" in {
    Stream(5, 6, 7, 8).takeWhileViaFoldRight(_ <= 7).toList should equal(List(5, 6, 7))
    Stream(5, 6, 7, 8).takeWhileViaFoldRight(_ < 3).toList should equal(List())
  }


}
