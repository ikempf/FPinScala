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

  "HeadOption" should "return the head of non empty streams" in {
    Stream(1, 4, 6, 7).headOption should equal(Some(1))
    Stream().headOption should equal(None)
  }

  "HeadOptionViaFoldRight" should "return the head of non empty streams" in {
    Stream(1, 4, 6, 7).headOptionViaFoldRight should equal(Some(1))
    Stream().headOptionViaFoldRight should equal(None)
  }

  "Map" should "apply given function to values of stream" in {
    Stream(1, 3, 9).map(_ * 5).toList should equal(List(5, 15, 45))
  }

  "Filter" should "filter elements not satisfying given predicate" in {
    Stream().filter((_) => true).toList should equal(List())
    Stream(4, 7, 8).filter(_ % 4 != 0).toList should equal(List(7))
  }

  "Append" should "append two stream" in {
    Stream(2, 6).append(Stream(3, 9)).toList should equal(List(2, 6, 3, 9))
  }

  "FlatMap" should "apply given function to elements of the stream" in {
    Stream(5, 10, 13).flatMap(Stream(_)).toList should equal(List(5, 10, 13))
  }

  "Constant" should "generate an infinite stream from a given value" in {
    Stream.constant("foo").take(3).toList should equal(List("foo", "foo", "foo"))
    Stream.constant(10).take(2).toList should equal(List(10, 10))
  }

  "From" should "generate an infinite icremental stream starting from the given value" in {
    Stream.from(10).take(4).toList should equal(List(10, 11, 12, 13))
  }

  "Fib" should "generate the fibonacci sequence" in {
    Stream.fib.take(10).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "OnesViaUnfold" should "generate an infinite stream of ones" in {
    Stream.onesViaUnfold.take(4).toList should equal(List(1, 1, 1, 1))
  }

  "ConstantViaUnfold" should "generate an infinite stream from a given value" in {
    Stream.constantViaUnfold("foo").take(3).toList should equal(List("foo", "foo", "foo"))
    Stream.constantViaUnfold(10).take(2).toList should equal(List(10, 10))
  }

  "FromViaUnfold" should "generate an infinite icremental stream starting from the given value" in {
    Stream.fromViaUnfold(10).take(4).toList should equal(List(10, 11, 12, 13))
  }

  "FibViaUnfold" should "generate the fibonacci sequence" in {
    Stream.fibViaUnfold.take(10).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "MapViaUnfold" should "apply given function to values of stream" in {
    Stream(1, 3, 9).mapViaUnfold(_ * 5).toList should equal(List(5, 15, 45))
  }

  "TakeViaUnfold" should "take n first elemnts of the stream" in {
    Stream(1).takeViaUnfold(2).toList should equal(List(1))
    Stream(1, 2, 3).takeViaUnfold(2).toList should equal(List(1, 2))
  }

  "TakeWhileViaUnfold" should "take starting elements respecting the given predicate" in {
    Stream(5, 6, 7, 8).takeWhileViaUnfold(_ <= 7).toList should equal(List(5, 6, 7))
    Stream(5, 6, 7, 8).takeWhileViaUnfold(_ < 3).toList should equal(List())
  }

  "ZipWith" should "combine elements of both streams" in {
    Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList should equal(List(5, 7, 9))
    Stream(1, 2).zipWith(Stream(4, 5, 6))(_ + _).toList should equal(List(5, 7))
    Stream(1, 2, 3, 4).zipWith(Stream(4, 5))(_ + _).toList should equal(List(5, 7))
  }

  "ZipAll" should "combine elements of both streams" in {
    Stream(1, 2).zipAll(Stream(4, 5)).toList should equal(List((Some(1), Some(4)), (Some(2), Some(5))))
    Stream(1, 2, 3).zipAll(Stream(4, 5)).toList should equal(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None)))
    Stream(1, 2).zipAll(Stream(4, 5, 6)).toList should equal(List((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6))))
  }

  "StartsWith" should "chech if stream starts with given stream" in {
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3)) should be(true)
    Stream(1, 2, 3, 4).startsWith(Stream(1, 3)) should be(false)
  }

  "Tails" should "return all suffixes of the stream" in {
    Stream.asList(Stream(1, 2, 3).tails) should equal(List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "HasSubsequence" should "be true if contains given sequence" in {
    Stream(4, 5, 6, 7, 10, 13).hasSubsequence(Stream(6, 7, 10)) should be(true)
  }

  //!TODO
  //  "ScanRight" should "scan the stream from right to left" in {
  //    Stream(1, 2, 3).scanRight(4)(_ + _).toList should equal(List(10, 9, 7, 4))
  //  }

  "ScanRightViaFoldRight" should "scan the stream from right to left" in {
    Stream(1, 2, 3).scanRightViaFoldRight(4)(_ + _).toList should equal(List(10, 9, 7, 4))
  }

}
