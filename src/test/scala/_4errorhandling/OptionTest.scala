package _4errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {

  "Map" should "apply function to option" in {
    None.map((a: Int) => a + 1) should equal(None)
    Some(5).map(_ + 1) should equal(Some(6))
  }

  "FlatMap" should "apply function to option and flatten result" in {
    None.flatMap((a: Int) => Some(a)) should equal(None)
    Some(3).flatMap((_) => None) should equal(None)
    Some(5).flatMap((a: Int) => Some(a * 2)) should equal(Some(10))
  }

  "GetOrElse" should "get value or default" in {
    None.getOrElse(5) should equal(5)
    None.getOrElse(None) should equal(None)
    Some(50).getOrElse(5) should equal(50)
    Some(List(1, 2)).getOrElse("azerty") should equal(List(1, 2))
  }

  "OrElse" should "get or default" in {
    None.orElse(Some(3)) should equal(Some(3))
    None.orElse(None) should equal(None)
    Some(2).orElse(None) should equal(Some(2))
  }

  "Filter" should "filter Some(x) if x does not satisfy predicate" in {
    None.filter((_) => true) should equal(None)
    Some(5).filter(_ > 3) should equal(Some(5))
    Some(5).filter(_ < 3) should equal(None)
  }

  "Mean" should "calculate the mean of the sequence" in {
    Option.mean(Seq(0, 5, 10)) should equal(Some(5))
    Option.mean(List(1, 2, 3, 4, 5)) should equal(Some(3))
  }

  "Variance" should "calculate variance of given sequence" in {
    Option.variance(List()) should equal(None)
    Option.variance(List(1, 2, 3, 4)) should equal(Some(1.25))
    Option.variance(List(1, 2, 3, 4, 5)) should equal(Some(2))
  }

  "Variance2" should "calculate variance of given sequence" in {
    Option.varianceBis(List()) should equal(None)
    Option.varianceBis(List(1, 2, 3, 4)) should equal(Some(1.25))
    Option.varianceBis(List(1, 2, 3, 4, 5)) should equal(Some(2))
  }

  "Map2" should "combine two options" in {
    Option.map2[Int, Int, Int](None, None)(_ + _) should equal(None)
    Option.map2[Int, Int, Int](None, Some(3))(_ + _) should equal(None)
    Option.map2(Some(4), Some(3))(_ + _) should equal(Some(7))
  }

  "Sequence" should "combine a list of options" in {
    Option.sequence(List(None, None, Some(3))) should equal(None)
    Option.sequence(List(Some(3), Some(2), Some(6))) should equal(Some(List(3, 2, 6)))
    Option.sequence(List()) should equal(Some(List()))
  }

  "SequenceViaFoldRight" should "combine a list of options" in {
    Option.sequenceViaFoldRight(List(None, None, Some(3))) should equal(None)
    Option.sequenceViaFoldRight(List(Some(3), Some(2), Some(6))) should equal(Some(List(3, 2, 6)))
    Option.sequenceViaFoldRight(List()) should equal(Some(List()))
  }

  "Traverse" should "sequnence and map givent list" in {
    Option.traverse(List("2", "5"))((a) => Option.Try(a.toInt)) should equal(Some(List(2, 5)))
    Option.traverse(List("2", "5", "a"))((a) => Option.Try(a.toInt)) should equal(None)
  }

}
