package state

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.Int.{MaxValue, MinValue}

class StateTest extends FlatSpec with Matchers with MockFactory {

  "NonNegativeInteger" should "generate non negative integer" in {
    state.nonNegativeInt(mockRNG(5))._1 should equal(5)
  }

  it should "generate non negative integer for negative integer" in {
    state.nonNegativeInt(mockRNG(-101))._1 should equal(101)
  }

  it should "generate non negative integer for min integer" in {
    state.nonNegativeInt(mockRNG(MinValue))._1 should equal(MaxValue)
  }

  "Double" should "generate a double in [0, 1[" in {
    state.double(mockRNG(9000000))._1 should equal(0.004190951585769653)
  }

  it should "generate a double in [0, 1[ from min integer" in {
    state.double(mockRNG(0))._1 should equal(0.0)
  }

  it should "generate a double in [0, 1[ from max integer" in {
    state.double(mockRNG(MaxValue))._1 should (be < 1.0 and be > 0.999)
  }

  it should "generate a double in [0, 1[ from negative integer" in {
    state.double(mockRNG(MinValue))._1 should (be < 1.0 and be > 0.999)
    state.double(mockRNG(9000000))._1 should equal(0.004190951585769653)
  }

  "IntDouble" should "generate a random Int Double pair" in {
    val r1 = mock[RNG]
    (r1.nextInt _).expects().returning((3, r1))
    (r1.nextInt _).expects().returning((0, r1))
    state.intDouble(r1) should equal((3, 0), r1)
  }

  "DoubleInt" should "generate a random Double Int pair" in {
    val r1 = mock[RNG]
    (r1.nextInt _).expects().returning((3, r1))
    (r1.nextInt _).expects().returning((0, r1))
    state.doubleInt(r1) should equal((0, 3), r1)
  }

  "Double3" should "generate a 3 random Doubles" in {
    val r1 = mock[RNG]
    (r1.nextInt _).expects().returning((0, r1))
    (r1.nextInt _).expects().returning((0, r1))
    (r1.nextInt _).expects().returning((0, r1))
    state.double3(r1) should equal((0, 0, 0), r1)
  }

  "Ints" should "generate a list of ints" in {
    val r1 = mock[RNG]
    (r1.nextInt _).expects().returning((4, r1))
    (r1.nextInt _).expects().returning((6, r1))
    (r1.nextInt _).expects().returning((10, r1))
    state.ints(3)(r1) should equal(List(10, 6, 4), r1)
  }

  "DoubleViaMap" should "generate a double in [0, 1[" in {
    state.doubleViaMap(mockRNG(9000000))._1 should equal(0.004190951585769653)
  }

  "IntsViaSequence" should "generate a list of ints" in {
    val r1 = mock[RNG]
    (r1.nextInt _).expects().returning((4, r1))
    (r1.nextInt _).expects().returning((6, r1))
    (r1.nextInt _).expects().returning((10, r1))
    state.intsViaSequence(3)(r1) should equal(List(10, 6, 4), r1)
  }

  def mockRNG(result: Int): RNG = {
    val rng1 = mock[RNG]
    (rng1.nextInt _).expects().returning((result, mock[RNG]))

    rng1
  }

}
