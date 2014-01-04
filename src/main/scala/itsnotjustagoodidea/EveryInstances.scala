package itsnotjustagoodidea // it's the law!

import scalaz._
import Scalaz._
import org.scalautils._


trait EveryInstances0 {
  implicit def everyEqual[A: Equal]: Equal[Every[A]] = Equal.equalBy[Every[A], List[A]](_.toList)(std.list.listEqual[A])
}

trait EveryInstances extends EveryInstances0 {
  implicit val every =
    new Traverse1[Every] with Monad[Every] with Plus[Every] with Comonad[Every] with Cobind.FromCojoin[Every] with Each[Every] with Zip[Every] with Unzip[Every] with Length[Every] {

/*
      def traverse1Impl[G[_] : Apply, A, B](fa: Every[A])(f: A => G[B]): G[Every[B]] =
        fa traverse1 f
*/

      // fa traverse1 f
      // def traverse1[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[NonEmptyList[B]] = {
      def traverse1Impl[F[_] : Apply, A, B](ea: Every[A])(g: A => F[B]): F[Every[B]] = {
        val efb: Every[F[B]] = ea map g
        efb match {
          case org.scalautils.One(fb) => implicitly[Apply[F]].map(fb)(org.scalautils.One(_))
          case org.scalautils.Many(fb1, fb2, fbs) => implicitly[Apply[F]].apply3(fb1, fb2, fbs) {
            case (h1, h2, t) => Many(h1, h2, t)
//def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
          }
        }
/*
        fa.tail.toList match {
          case Nil => F.map(g(head))(nel(_, Nil))
          case b :: bs => F.apply2(f(head), OneAnd.oneAndTraverse[List].traverse1(OneAnd(b, bs))(g)) {
            case (h, t) => nel(h, t.head :: t.tail)
          }
        }
*/
      }

      override def foldRight1[A](fa: Every[A])(f: (A, => A) => A): A = {
        val reversed = fa.reverse
        reversed.tail.foldLeft(reversed.head)((x, y) => f(y, x))
      }

      override def foldLeft1[A](fa: Every[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def foldMap1[A, B](fa: Every[A])(f: A => B)(implicit F: Semigroup[B]): B = {
        fa.tail.foldLeft(f(fa.head))((x, y) => F.append(x, f(y)))
      }

      override def foldLeft[A, B](fa: Every[A], z: B)(f: (B, A) => B): B =
        fa.tail.foldLeft(f(z, fa.head))(f)

      def bind[A, B](fa: Every[A])(f: A => Every[B]): Every[B] = fa flatMap f

      def point[A](a: => A): Every[A] = Every(a)

      def plus[A](a: Every[A], b: => Every[A]): Every[A] = a ++ b

      def copoint[A](p: Every[A]): A = p.head

      def cojoin[A](a: Every[A]): Every[Every[A]] = ???
/*
def tails: NonEmptyList[NonEmptyList[A]] = nel(this, tail match {
    case Nil    => Nil
    case h :: t => nel(h, t).tails.list
  })
*/

      def each[A](fa: Every[A])(f: A => Unit) = fa.toList foreach f

      def zip[A, B](a: => Every[A], b: => Every[B]): Every[(A, B)] = Every.from(a zip b).get

      def unzip[A, B](a: Every[(A, B)]) = a.unzip

      def length[A](a: Every[A]): Int = a.size
    }

 implicit def everySemigroup[A]: Semigroup[Every[A]] = new Semigroup[Every[A]] {
    def append(f1: Every[A], f2: => Every[A]) = f1 ++ f2
  }

  implicit def everyShow[A: Show]: Show[Every[A]] = new Show[Every[A]] {
    import std.list._
    override def show(fa: Every[A]) = Show[List[A]].show(fa.toList)
  }

  implicit def everyOrder[A: Order]: Order[Every[A]] =
    Order.orderBy[Every[A], List[A]](_.toList)(std.list.listOrder[A])
}

object EveryInstances extends EveryInstances

