
package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._

/*
trait LowPriorityOrInstances {

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

  implicit def orMonad[B]: Monad[({type l[g]=g Or B})#l] = new Monad[({type l[g]=g Or B})#l] {
    def point[G](g: => G) = Good(g)
    def bind[G, H](fa: G Or B)(f: G => H Or B) = fa flatMap f
  }
}

trait OrInstances extends OrInstances0 {

  implicit class BadMapper[G, B](or: Or[G, B]) {
    def badMap[C](bToC: B => C): G Or C = or.swap.map(bToC).swap
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
*/

/*
sealed abstract class OrInstances3 {
  implicit val OrInstances3 : Bitraverse[Or] = new Bitraverse[Or] {
    override def bimap[G, B, Y, D](fab: G Or B)
                                  (f: G => Y, g: B => D) = fab bimap (f, g)

    def bitraverseImpl[Z[_] : Applicative, G, B, Y, D](fab: G Or B)
                                                  (f: G => Z[Y], g: B => Z[D]) =
      fab.bitraverse(f, g)
  }
}

sealed abstract class OrInstances2 extends OrInstances3 {
  implicit def OrInstances2[L]: Traverse[({type l[a] = L Or a})#l] with Monad[({type l[a] = L Or a})#l] with Cozip[({type l[a] = L Or a})#l] with Plus[({type l[a] = L Or a})#l] with Optional[({type l[a] = L Or a})#l] = new Traverse[({type l[a] = L Or a})#l] with Monad[({type l[a] = L Or a})#l] with Cozip[({type l[a] = L Or a})#l] with Plus[({type l[a] = L Or a})#l] with Optional[({type l[a] = L Or a})#l] {
    def bind[G, B](fa: L Or G)(f: G => L Or B) =
      fa flatMap f

    def point[G](a: => G) =
      \/-(a)

    def traverseImpl[Z[_] : Applicative, G, B](fa: L Or G)(f: G => Z[B]) =
      fa.traverse(f)

    override def foldRight[G, B](fa: L Or G, z: => B)(f: (G, => B) => B) =
      fa.foldRight(z)(f)

    def cozip[G, B](x: L Or (G Or B)) =
      x match {
        case l @ -\/(_) => -\/(l)
        case \/-(e) => e match {
          case -\/(a) => -\/(\/-(a))
          case b @ \/-(_) => \/-(b)
        }
      }

    def plus[G](a: L Or G, b: => L Or G) =
      a orElse b

    def pextract[B, G](fa: L Or G): (L Or B) Or G = fa match {
      case l@ -\/(_) => -\/(l)
      case r@ \/-(_) => r
    }
  }
}
*/

trait OrInstances2 { // extends OrInstances3 {
  implicit def orMonad[B]: Monad[({type l[g]=g Or B})#l] = new Monad[({type l[g]=g Or B})#l] {
    def point[G](g: => G) = Good(g)
    def bind[G, H](fa: G Or B)(f: G => H Or B) = fa flatMap f
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

/*
sealed abstract class OrInstances3 {
  implicit val OrInstances3 : Bitraverse[Or] = new Bitraverse[Or] {
    override def bimap[G, B, Y, D](fab: G Or B)
                                  (f: G => Y, g: B => D) = fab bimap (f, g)

    def bitraverseImpl[Z[_] : Applicative, G, B, Y, D](fab: G Or B)
                                                  (f: G => Z[Y], g: B => Z[D]) =
      fab.bitraverse(f, g)
  }
}

sealed abstract class OrInstances2 extends OrInstances3 {
  implicit def OrInstances2[L]: Traverse[({type l[a] = L Or a})#l] with Monad[({type l[a] = L Or a})#l] with Cozip[({type l[a] = L Or a})#l] with Plus[({type l[a] = L Or a})#l] with Optional[({type l[a] = L Or a})#l] = new Traverse[({type l[a] = L Or a})#l] with Monad[({type l[a] = L Or a})#l] with Cozip[({type l[a] = L Or a})#l] with Plus[({type l[a] = L Or a})#l] with Optional[({type l[a] = L Or a})#l] {
    def bind[G, B](fa: L Or G)(f: G => L Or B) =
      fa flatMap f

    def point[G](a: => G) =
      \/-(a)

    def traverseImpl[Z[_] : Applicative, G, B](fa: L Or G)(f: G => Z[B]) =
      fa.traverse(f)

    override def foldRight[G, B](fa: L Or G, z: => B)(f: (G, => B) => B) =
      fa.foldRight(z)(f)

    def cozip[G, B](x: L Or (G Or B)) =
      x match {
        case l @ -\/(_) => -\/(l)
        case \/-(e) => e match {
          case -\/(a) => -\/(\/-(a))
          case b @ \/-(_) => \/-(b)
        }
      }

    def plus[G](a: L Or G, b: => L Or G) =
      a orElse b

    def pextract[B, G](fa: L Or G): (L Or B) Or G = fa match {
      case l@ -\/(_) => -\/(l)
      case r@ \/-(_) => r
    }
  }
}

sealed abstract class OrInstances1 extends OrInstances2 {
  implicit def OrEqual[G: Equal, B: Equal]: Equal[G Or B] =
    new Equal[G Or B] {
      def equal(a1: G Or B, a2: G Or B) =
        a1 === a2
    }

  implicit def OrShow[G: Show, B: Show]: Show[G Or B] =
    Show.show(_.show)

  implicit def OrSemigroup[G: Semigroup, B: Semigroup]: Semigroup[G Or B] =
    new Semigroup[G Or B] {
      def append(a1: G Or B, a2: => G Or B) =
        a1 +++ a2
    }
}

sealed abstract class OrInstances0 extends OrInstances1 {
  implicit def orOrder[G : Order, B : Order]: Order[G Or B] =
    new Order[G Or B] {
      def order(a1: G Or B, a2: G Or B) =
        a1 compare a2
    }

  implicit def orMonoid[G: Semigroup, B: Monoid]: Monoid[G Or B] =
    new Monoid[G Or B] {
      def append(a1: G Or B, a2: => G Or B) =
        a1 +++ a2
      def zero =
        \/-(Monoid[B].zero)
    }
}

sealed abstract class OrInstances extends OrInstances0 {
  type GlorifiedTuple[+G, +B] =
  G \/ B
}
*/

