package monoid

import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  "FoldMap" should "concatenate list" in {
    // Given
    val doubles = List(5d, 3d, 1d)

    // When
    val concat = Monoid.foldMap(doubles, MonoidInstances.stringMonoid)(_.toInt.toString)

    // Then
    concat should equal("531")
  }

  "FoldRightMap" should "fold right" in {
    // Given
    val l = List(5, 5, 3, 9)

    // When
    val concat = Monoid.foldRightMap(l)("Res:")((a, b) => b + a)

    // Then
    concat should equal("Res:9355")
  }

  "FoldRightMapSteps" should "fold right" in {
    // Given
    val l = List(5, 5, 3, 9)

    // When
    val concat = Monoid.foldRightMapSteps(l)("Res:")((a, b) => b + a)

    // Then
    concat should equal("Res:9355")
  }

  "FoldLeftMap" should "fold left" in {
    // Given
    val l = List(5, 5, 3, 9)

    // When
    val concat = Monoid.foldLeftMap(l)("Res:")(_ + _)

    // Then
    concat should equal("Res:5539")
  }

  "FoldLeftMapSteps" should "fold left" in {
    // Given
    val l = List(5, 5, 3, 9)

    // When
    val concat = Monoid.foldLeftMapSteps(l)("Res:")(_ + _)

    // Then
    concat should equal("Res:5539")
  }

}
