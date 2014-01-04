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
import EveryInstances._
import Gen.oneOf
import Arbitrary.arbitrary
import Arbitrary.arbContainer

class AccumulatingOrLawsSpec extends PropSpec with Checkers {

  def checkAll(prefix: String, props: Properties) {
    for (((name, prop), idx) <- props.properties.zipWithIndex) {
      property(prefix + idx + ": " + name) { check(prop) }
    }
  }

  implicit def everyArbitrary[E](implicit ae: Arbitrary[E]): Arbitrary[Every[E]] = {
    def oneE: One[E] =
      ae.arbitrary.sample match {
        case Some(e) => One(e)
        case None => oneE // feeling lucky
      }
    Arbitrary { 
      arbContainer[List, E].arbitrary.map {
        Every.from(_) match {
          case None => oneE
          case Some(e) => e
        }
      }
    }
  }
  implicit def orArbitrary[G, B](implicit ag: Arbitrary[G], ab: Arbitrary[B]): Arbitrary[G Or B] =
    Arbitrary(oneOf(arbitrary[G].map(Good(_)), arbitrary[B].map(Bad(_))))

  checkAll("order", order.laws[Int Or Every[Int]])
  checkAll("monoid", monoid.laws[Int Or Every[Int]])
  checkAll("monad", monad.laws[({type l[g] = g Or Every[Int]})#l])
  checkAll("plus", plus.laws[({type l[g] = g Or Every[Int]})#l])
  checkAll("traverse", traverse.laws[({type l[g] = g Or Every[Int]})#l])
  checkAll("bitraverse", bitraverse.laws[Or])
  checkAll("applicative", applicative.laws[({type l[g] = g Or Every[Int]})#l])
}

