/*
 * Copyright 2014 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

class OrLawsSpec extends LawsSpec {

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

