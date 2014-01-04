
package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._

trait LowPriorityOrInstances {
  implicit def orMonad[B]: Monad[({type l[g]=g Or B})#l] = new Monad[({type l[g]=g Or B})#l] {
    def point[G](g: => G) = Good(g)
    def bind[G, H](fa: G Or B)(f: G => H Or B) = fa flatMap f
  }
}

trait OrInstances extends LowPriorityOrInstances {

  implicit class BadMapper[G, B](or: Or[G, B]) {
    def badMap[C](bToC: B => C): G Or C = or.swap.map(bToC).swap
  }

  implicit def orEqual[G : Equal, B : Equal]: Equal[G Or B] =
    new Equal[G Or B] {
      def equal(or1: G Or B, or2: G Or B) = Equivalence.default.areEquivalent(or1, or2)
    }

  implicit def accumulatingOrApplicative[G, B, E[b] <: Every[b]]: Applicative[({type l[g] = g Or Every[B]})#l] =
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
}

object OrInstances extends OrInstances
