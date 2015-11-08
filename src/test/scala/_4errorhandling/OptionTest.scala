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

}
