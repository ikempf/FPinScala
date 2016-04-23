package datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  "Tail" should "return the tail of the list" in {
    List.tail(List(1, 2, 3)) should equal(List(2, 3))
  }

  it should "throw when list is Nil" in {
    intercept[IllegalArgumentException] {
      List.tail(Nil)
    }
  }

  "SetHead" should "replace the head of the list" in {
    List.setHead(List(1, 2, 3), 5) should equal(List(5, 2, 3))
  }

  it should "throw when list is Nil" in {
    intercept[IllegalArgumentException] {
      List.setHead(Nil, 2)
    }
  }

  "Drop" should "drop the n first elements of the list" in {
    List.drop(List(1, 2, 3, 4, 5), 2) should equal(List(3, 4, 5))
  }

  "DropWhile" should "drop elements while the condition is satified" in {
    List.dropWhile(List(1, 2, 3, 4, 5, 6), (a: Int) => a < 4) should equal(List(4, 5, 6))
  }

  "Init" should "drop the last element" in {
    List.init(List(1, 2, 3, 4, 5)) should equal(List(1, 2, 3, 4))
  }

  "ProductFoldRight" should "process the product of the elements of the list" in {
    List.productFoldRight(List(1, 2, 3, 4, 10)) should equal(240)
  }

  "ReconstructList" should "do something" in {
    List.reconstructList(List(1, 2, 3, 4)) should equal(List(1, 2, 3, 4))
  }

  "Length" should "return length of the list" in {
    List.length(List(1, 2, 3)) should equal(3)
  }

  "FoldLeft" should "should fold list from left to right" in {
    List.foldLeft(List(1, 2, 3, 4), Nil: List[Int])((b, a) => List.append(Cons(a, Nil), b)) should equal(List(4, 3, 2, 1))
  }

  "ProductFoldLeft" should "process the product of the elements of the list" in {
    List.productFoldLeft(List(1, 2, 3, 4, 10)) should equal(240)
  }

  "SumFoldLeft" should "process the sum of the elements of the list" in {
    List.sumFoldLeft(List(1, 2, 3, 4, 5)) should equal(15)
  }

  "LengthFoldLeft" should "return length of the list" in {
    List.lengthFoldLeft(List(1, 2, 3)) should equal(3)
  }

  "Reverse" should "reverse the lsit" in {
    List.reverse(List(1, 2, 3)) should equal(List(3, 2, 1))
  }

  "ReverseFoldLeft" should "reverse the lsit" in {
    List.reverseFoldLeft(List(1, 2, 3)) should equal(List(3, 2, 1))
  }

  "FoldRightViaFoldLeft" should "fold list from right to left" in {
    List.foldRightViaFoldLeft(List(5, 6, 7), Nil: List[Int])((a, b) => Cons(a, b)) should equal(List(5, 6, 7))
  }

  "Append" should "append given lists" in {
    List.appendViaFoldLeft(List(1, 2, 3), List(10, 11)) should equal(List(1, 2, 3, 10, 11))
  }

  "AppendViaFoldRight" should "append given lists" in {
    List.appendViaFoldRight(List(1, 2, 3), List(10, 11)) should equal(List(1, 2, 3, 10, 11))
  }

  "Concat" should "concatenate list of lists" in {
    List.concat(List(List(1, 2, 3), List(10, 11), List(5))) should equal(List(1, 2, 3, 10, 11, 5))
  }

  "Add1" should "add one to each element of the list" in {
    List.add1(List(10, 11, 12)) should equal(List(11, 12, 13))
  }

  "ToStrings" should "transforms list of doubles to list of strings" in {
    List.toStrings(List(5d, 6d, 7d)) should equal(List("5.0", "6.0", "7.0"))
  }

  "Map" should "update each element of the list" in {
    List.map(List(4, 5, 6))(_ * 2) should equal(List(8, 10, 12))
  }

  "Filter" should "only keep elements that satisfy given condition" in {
    List.filter(List(3, 6, 10))(_ % 3 == 0) should equal(List(3, 6))
  }

  "FlatMap" should "should update each element of the list and flatten the result" in {
    List.flatMap(List(3, 4, 10))(a => List(a)) should equal(List(3, 4, 10))
    List.flatMap(List(3, 4, 10))(a => List(a, a)) should equal(List(3, 3, 4, 4, 10, 10))
  }

  "FilterViaFlatMap" should "filter elements not satisfying the given predicate" in {
    List.filterViaFlatMap(List(1, 2, 3))(a => a == 1) should equal(List(1))
    List.filterViaFlatMap(List(1, 2, 3))(a => a > 1) should equal(List(2, 3))
  }

  "AddLists" should "create a new list by adding corresponding elements" in {
    List.addLists(List(1, 2, 3), List(4, 5, 6)) should equal(List(5, 7, 9))
  }

  "Zip" should "create a new list by applying given binary function to elemnts of the lists" in {
    List.zip(List(1, 2, 3), List(4, 5, 6))(_ + _) should equal(List(5, 7, 9))
  }

  "HasSubsequence" should "return true if the given list contains the given subsequence" in {
    List.hasSubsequence(List(4, 5, 6, 1, 4, 5), List(9, 4, 7)) should equal(false)
    List.hasSubsequence(List(4, 5, 6, 1, 4, 5), List(5, 6, 1)) should equal(true)
  }

}
