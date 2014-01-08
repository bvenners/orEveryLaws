package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._

trait OrInstances3 {
  implicit val orInstances3 : Bitraverse[Or] = new Bitraverse[Or] {
    override def bimap[G, B, H, C](gorb: G Or B)(gf: G => H, bf: B => C): H Or C =
      gorb match {
        case Good(a) => Good(gf(a))
        case Bad(b) => Bad(bf(b))
      }

    def bitraverseImpl[Z[_] : Applicative, G, B, H, C](gorb: G Or B)(gf: G => Z[H], bf: B => Z[C]) =
      gorb match {
        case Good(g) => Functor[Z].map(gf(g))(Good(_))
        case Bad(b) => Functor[Z].map(bf(b))(Bad(_))
      }
  }
}

trait OrInstances2 extends OrInstances3 {

  implicit def orInstances2[B]: Traverse[({type l[g] = g Or B})#l] with Monad[({type l[g] = g Or B})#l] with Cozip[({type l[g] = g Or B})#l] with Plus[({type l[g] = g Or B})#l] = new Traverse[({type l[g] = g Or B})#l] with Monad[({type l[g] = g Or B})#l] with Cozip[({type l[g] = g Or B})#l] with Plus[({type l[g] = g Or B})#l] {

    def bind[G, H](fa: G Or B)(f: G => H Or B) =
      fa flatMap f

    def point[G](g: => G) =
      Good(g)

    def traverseImpl[Z[_] : Applicative, G, Y](fa: G Or B)(f: G => Z[Y]): Z[Y Or B] = 
      fa match {
        case Bad(b) => Applicative[Z].point(Good[Y].orBad(b)) 
        case Good(g) => Functor[Z].map(f(g))(Good(_))
      }

    override def foldRight[G, C](fa: G Or B, z: => C)(f: (G, => C) => C) =
      fa.foldRight(z)(f)

    def cozip[C, G](x: (C \/ G) Or B): ((C Or B) \/ (G Or B)) = ???
/*
      x match {
        case b @ Bad(_) => Bad(b)
        case Good(e) => e match {
          case Bad(a) => Bad(Good(a))
          case b @ Good(_) => Good(b)
          case -\/(c) => Bad(\/-(c))
          case b @ \/-(_) => \/-(b)
        }
      }
*/

    def plus[G](a: G Or B, b: => G Or B) =
      a orElse b

    override def toString = "the other monad one"
  }
}

trait OrInstances1 extends OrInstances2 {
  implicit def orEqual[G : Equal, B : Equal]: Equal[G Or B] =
   new Equal[G Or B] {
     def equal(or1: G Or B, or2: G Or B) = Equivalence.default.areEquivalent(or1, or2)
   }

/*
  implicit def OrShow[G: Show, B: Show]: Show[G Or B] =
    Show.show(_.show)

  implicit def OrSemigroup[G: Semigroup, B: Semigroup]: Semigroup[G Or B] =
    new Semigroup[G Or B] {
      def append(a1: G Or B, a2: => G Or B) =
        a1 +++ a2
    }
*/
}

trait OrInstances0 extends OrInstances1 {
  implicit def orOrder[G : Order, B : Order]: Order[G Or B] =
    new Order[G Or B] {
      def order(or1: G Or B, or2: G Or B) = {
        or1 match {
          case Bad(b1) => or2 match {
            case Bad(b2) => Order[B].apply(b1, b2)
            case Good(_) => Ordering.LT
          }
          case Good(g1) => or2 match {
            case Good(g2) => Order[G].apply(g1, g2)
            case Bad(_) => Ordering.GT
          }
        }
      }
    }

  implicit def orMonoid[G : Monoid, B : Semigroup]: Monoid[G Or B] =
    new Monoid[G Or B] {
      def append(or1: G Or B, or2: => G Or B) = {
        or1 match {
          case Bad(b1) => or2 match {
            case Bad(b2) => Bad(Semigroup[B].append(b1, b2))
            case Good(_) => or1
          }
          case Good(g1) => or2 match {
            case b2 @ Bad(_) => b2
            case Good(g2) => Good(Semigroup[G].append(g1, g2))
          }
        }
      }
      def zero =
        Good(Monoid[G].zero)
    }
}

trait OrInstances extends OrInstances0 {
  implicit class BadMapper[G, B](or: Or[G, B]) {
    def badMap[C](bToC: B => C): G Or C = or.swap.map(bToC).swap
  }

  implicit def accumulatingOrApplicative[G, B]: Applicative[({type l[g] = g Or Every[B]})#l] =
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

  def accumulatingOrApplicativeForSemigroup[B : Semigroup]: Applicative[({type l[g] = g Or B})#l] =
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

object OrInstances extends OrInstances

