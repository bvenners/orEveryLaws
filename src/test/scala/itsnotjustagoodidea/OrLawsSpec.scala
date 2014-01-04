package itsnotjustagoodidea // It's the law!

import org.scalatest._
import prop.Checkers
import org.scalautils._
import org.scalacheck._
import org.scalacheck.Prop.Result
import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import OrInstances._
import Gen.oneOf
import Arbitrary.arbitrary

class OrLawsSpec extends PropSpec with Checkers {

  def checkAll(prefix: String, props: Properties) {
    for (((name, prop), idx) <- props.properties.zipWithIndex) {
      property(prefix + idx + ": " + name) { check(prop) }
    }
  }

  implicit def orArbitrary[G, B](implicit ag: Arbitrary[G], ab: Arbitrary[B]): Arbitrary[G Or B] =
    Arbitrary(oneOf(arbitrary[G].map(Good(_)), arbitrary[B].map(Bad(_))))

  checkAll("order", order.laws[Int Or Int])
  checkAll("monoid", monoid.laws[Int Or Int])
  checkAll("monad", monad.laws[({type l[g] = g Or Int})#l])
  checkAll("plus", plus.laws[({type l[g] = g Or Int})#l])
  checkAll("traverse", traverse.laws[({type l[g] = g Or Int})#l])
  checkAll("bitraverse", bitraverse.laws[Or])
  checkAll("applicative", applicative.laws[({type l[g] = g Or Int})#l])
}

