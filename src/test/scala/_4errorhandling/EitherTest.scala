package _4errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "Map" should "apply function for Right" in {
    Right(50).map(_ * 5) should equal(Right(250))
    Left(-1).map(Nothing => 5) should equal(Left(-1))
  }

  "FlatMap" should "apply function for Right" in {
    Right(50).map(_ * 5) should equal(Right(250))
    Left(-1).map(Nothing => 5) should equal(Left(-1))
  }

  "OrElse" should "return Right or else default" in {
    Right(10).orElse(Right(3)) should equal(Right(10))
    Left(10).orElse(Right(3)) should equal(Right(3))
    Left(10).orElse(Left(3)) should equal(Left(3))
  }

  "Map2" should "combine two Either" in {
    Right(10).map2(Right(3))(_+_) should equal(Right(13))
    Right(10).map2(Left(3))(_+_) should equal(Left(3))
    Left(5).map2(Left(3))((Nothing, x) => x) should equal(Left(5))
  }

  "Map2ViaForComprehension" should "combine two Either" in {
    Right(10).map2ViaForComprehension(Right(3))(_+_) should equal(Right(13))
    Right(10).map2ViaForComprehension(Left(3))(_+_) should equal(Left(3))
    Left(5).map2ViaForComprehension(Left(3))((Nothing, x) => x) should equal(Left(5))
  }

}
