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

  def checkAll(props: Properties) {
    for ((name, prop) <- props.properties) yield {
      property(name) { check(prop) }
    }
  }

  implicit def orArbitrary[G, B](implicit ag: Arbitrary[G], ab: Arbitrary[B]): Arbitrary[G Or B] =
    Arbitrary(oneOf(arbitrary[G].map(Good(_)), arbitrary[B].map(Bad(_))))

  checkAll(monad.laws[({type l[g] = g Or Int})#l])
}

