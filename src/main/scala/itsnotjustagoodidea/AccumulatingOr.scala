package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._


trait AccumulatingOr {

  trait MoreSpecificApplicative[T[_]] extends Applicative[T]

  implicit def applicativeForEvery[G, B]: MoreSpecificApplicative[({type l[g] = g Or Every[B]})#l] =
    new MoreSpecificApplicative[({type l[g] = g Or Every[B]})#l] {
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

