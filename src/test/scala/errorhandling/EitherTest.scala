package errorhandling

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

  "Sequence" should "combine a list of Either" in {
    Either.sequence(List(Right(10), Right(2), Right(4))) should equal (Right(List(10, 2, 4)))
    Either.sequence(List(Right(10), Left("error"), Right(4))) should equal (Left("error"))
    Either.sequence(List(Left("error1"), Right(2), Left("error2"))) should equal (Left("error1"))
  }

  val eitherFromInt = (a:Int) => if (a < 5) Left("error") else Right(a*2)
  "Traverse" should "combine a list of Either and apply given function" in {
    Either.traverse(List(10, 7, 13))(eitherFromInt) should equal (Right(List(20, 14, 26)))
    Either.traverse(List(3, 7, 8))(eitherFromInt) should equal (Left("error"))
  }

  "TraverseViaFoldRight" should "combine a list of Either and apply given function" in {
    Either.traverse(List(10, 7, 13))(eitherFromInt) should equal (Right(List(20, 14, 26)))
    Either.traverse(List(3, 7, 8))(eitherFromInt) should equal (Left("error"))
  }

}
