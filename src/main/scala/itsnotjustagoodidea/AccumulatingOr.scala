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


trait AccumulatingOr {

  implicit def applicativeForEvery[G, B]: Applicative[({type l[g] = g Or Every[B]})#l] =
    new Applicative[({type l[g] = g Or Every[B]})#l] {
      override def map[G, H](fa: G Or Every[B])(f: G => H) = fa map f
  
      def point[G](g: => G) = Good(g)
 
      def ap[G, H](fa: => G Or Every[B])(f: => ((G => H) Or Every[B])): H Or Every[B] =
        (fa, f) match {
          case (Good(g), Good(f)) => Good(f(g))
          case (Bad(b), Good(_)) => Bad(b)
          case (Good(f), Bad(b)) => Bad(b)
          case (Bad(b1), Bad(b2)) => Bad(b1 ++ b2)
        }
    }

  def applicativeFor[B : Semigroup]: Applicative[({type l[g] = g Or B})#l] =
    new Applicative[({type l[g] = g Or B})#l] {
      override def map[G, H](fa: G Or B)(f: G => H) = fa map f
  
      def point[G](g: => G) = Good(g)
 
      def ap[G, H](fa: => G Or B)(f: => ((G => H) Or B)): H Or B =
        (fa, f) match {
          case (Good(g), Good(f)) => Good(f(g))
          case (Bad(b), Good(_)) => Bad(b)
          case (Good(f), Bad(b)) => Bad(b)
          case (Bad(b1), Bad(b2)) => Bad(Semigroup[B].append(b1, b2))
        }

      override def toString = "my applicative"
    }
}

object AccumulatingOr extends AccumulatingOr

