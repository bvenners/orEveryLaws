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
package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._

trait AccumulatingDisjunction {

  implicit def applicativeForEvery[G, B]: Applicative[({type l[g] = Every[B] \/ g})#l] =
    new Applicative[({type l[g] = Every[B] \/ g})#l] {
      override def map[G, H](fa: Every[B] \/ G)(f: G => H) = fa map f
  
      def point[G](g: => G) = \/-(g)
 
      def ap[G, H](fa: => Every[B] \/ G)(f: => (Every[B] \/ (G => H))): Every[B] \/ H =
        (fa, f) match {
          case (\/-(g), \/-(f)) => \/-(f(g))
          case (-\/(b), \/-(_)) => -\/(b)
          case (\/-(f), -\/(b)) => -\/(b)
          case (-\/(b1), -\/(b2)) => -\/(b1 ++ b2)
        }
    }

  def applicativeFor[B : Semigroup]: Applicative[({type l[g] = B \/ g})#l] =
    new Applicative[({type l[g] = B \/ g})#l] {
      override def map[G, H](fa: B \/ G)(f: G => H) = fa map f
  
      def point[G](g: => G) = \/-(g)
 
      def ap[G, H](fa: => B \/ G)(f: => (B \/ (G => H))): B \/ H =
        (fa, f) match {
          case (\/-(g), \/-(f)) => \/-(f(g))
          case (-\/(b), \/-(_)) => -\/(b)
          case (\/-(f), -\/(b)) => -\/(b)
          case (-\/(b1), -\/(b2)) => -\/(Semigroup[B].append(b1, b2))
        }

      override def toString = "my applicative"
    }
}

object AccumulatingDisjunction extends AccumulatingDisjunction

