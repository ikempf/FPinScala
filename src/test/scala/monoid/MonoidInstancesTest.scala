package monoid

import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MonoidInstancesTest extends FlatSpec with Matchers with PropertyChecks {

  def monoidLaws[A](monoid: Monoid[A])(implicit arbitrary: Arbitrary[A]) = {
    forAll {
      (a1: A, a2: A, a3: A) => {
        // Associativity
        monoid.op(a1, monoid.op(a2, a3)) should equal(monoid.op(monoid.op(a1, a2), a3))

        // Left identity
        monoid.op(monoid.zero, a1) should equal(a1)

        // Right identity
        monoid.op(a1, monoid.zero) should equal(a1)
      }
    }
  }

  "StringMonoid" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.stringMonoid)
  }

  "ListMonoid" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.listMonoid[Int])
  }

  "IntAddition" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.intAddition)
  }

  "IntMultiplication" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.intMultiplication)
  }

  "BooleanOr" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.booleanOr)
  }

  "BooleanAnd" should "obey monoid laws" in {
    monoidLaws(MonoidInstances.booleanAnd)
  }

  "EndoMonoid" should "obey monoid laws" in {
    val monoid = MonoidInstances.endoMonoid[String]
    forAll {
      (a1: String => String, a2: String => String, a3: String => String, applied: String) => {
        // Associativity
        monoid.op(a1, monoid.op(a2, a3))(applied) should equal(monoid.op(monoid.op(a1, a2), a3)(applied))

        // Left identity
        monoid.op(monoid.zero, a1)(applied) should equal(a1(applied))

        // Right identity
        monoid.op(a1, monoid.zero)(applied) should equal(a1(applied))
      }
    }
  }

}
