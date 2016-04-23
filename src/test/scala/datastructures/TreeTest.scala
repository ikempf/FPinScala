package datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  "Size" should "return the number of nodes of the tree" in {
    Tree.size(Branch(Leaf(5), Branch(Leaf(1), Leaf("a")))) should equal(5)
  }

  "Max" should "return the max leaf value" in {
    Tree.max(Branch(Leaf(5), Branch(Branch(Leaf(10), Leaf(4)), Leaf(2)))) should equal(10)
  }

  "Depth" should "return depth of the tree" in {
    Tree.depth(Branch(Leaf(5), Branch(Branch(Leaf(10), Leaf(4)), Leaf(2)))) should equal(3)
  }

  "Map" should "apply given function to each element of the tree" in {
    Tree.map(Leaf(4))(_.toString) should equal(Leaf("4"))
    Tree.map(Branch(Leaf(4d), Branch(Leaf(2d), Leaf(1d))))(_ / 2) should equal(Branch(Leaf(2), Branch(Leaf(1), Leaf(0.5))))
  }

  "Fold" should "fold tree (depth first search)" in {
    Tree.fold(Branch(Leaf(5), Branch(Leaf(1), Leaf(3))))(n => n)(_ + _) should equal(9)
  }

  "SizeViaFOld" should "return the number of nodes of the tree" in {
    Tree.sizeViaFold(Branch(Leaf(5), Branch(Leaf(1), Leaf("a")))) should equal(5)
  }

  "MaxViaFold" should "return the max leaf value" in {
    Tree.maxViaFold(Branch(Leaf(5), Branch(Branch(Leaf(10), Leaf(4)), Leaf(2)))) should equal(10)
  }

  "DepthViaFold" should "return depth of the tree" in {
    Tree.depthViaFold(Branch(Leaf(5), Branch(Branch(Leaf(10), Leaf(4)), Leaf(2)))) should equal(3)
  }

  "MapViaFold" should "apply given function to each element of the tree" in {
    Tree.mapViaFold(Leaf(4))(_.toString) should equal(Leaf("4"))
    Tree.mapViaFold(Branch(Leaf(4d), Branch(Leaf(2d), Leaf(1d))))(_ / 2) should equal(Branch(Leaf(2), Branch(Leaf(1), Leaf(0.5))))
  }

  "FoldWithAcc" should "fold tree and return accumulated result" in {
    Tree.foldWithAcc(
      Branch(Leaf(5), Branch(Branch(Leaf(10), Leaf(4)), Leaf(2))),
      Nil: List[Int]
    )(Cons(_, _))(List.append) should equal(List(5, 10, 4, 2))
  }

}
