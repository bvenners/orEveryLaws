package itsnotjustagoodidea // It's the law!

import scalaz._
import Scalaz._
import org.scalatest._
import org.scalautils._
import OrInstances._
import EveryInstances._

class ExampleSpec extends WordSpec with Matchers {

  def all[F[_]: Applicative, A](s: String, parsers: List[String => F[A]]): F[List[A]] = parsers.traverse(p => p(s))

  def toIntE(s: String): Int Or ErrorMessage = attempt(s.toInt).badMap(_.getMessage)

  def one: String => (Int Or List[ErrorMessage]) = s => toIntE(s).badMap(List(_))
  def two: String => (Int Or List[ErrorMessage]) = s => toIntE(s.take(2)).badMap(List(_))
  def three: String => (Int Or List[ErrorMessage]) = s => toIntE(s.drop(2)).badMap(List(_))

  def oneA: String => (Int Or Every[ErrorMessage]) = s => toIntE(s).badMap(Every(_))
  def twoA: String => (Int Or Every[ErrorMessage]) = s => toIntE(s.take(2)).badMap(Every(_))
  def threeA: String => (Int Or Every[ErrorMessage]) = s => toIntE(s.drop(2)).badMap(Every(_))

  "Or" should {
    "short circuit at the first error when the bad type is not an Every" in {
      val a = all[({ type l[a] = a Or List[ErrorMessage] })#l, Int]("1234", List(one, two, three))
      a shouldEqual Good(List(1234, 12, 34))

      val b = all[({ type l[a] = a Or List[ErrorMessage] })#l, Int]("asfasf", List(one, two, three))
      b shouldEqual Bad(List("For input string: \"asfasf\""))
    }

    "accumulate errors when the bad type is an Every" in {
      val c = all[({ type l[a] = a Or Every[ErrorMessage] })#l, Int]("1234", List(oneA, twoA, threeA))
      c shouldEqual Good(List(1234, 12, 34))

      val d = all[({ type l[a] = a Or Every[ErrorMessage] })#l, Int]("asfasf", List(oneA, twoA, threeA))
      d shouldEqual Bad(Many("For input string: \"fasf\"", "For input string: \"as\"", "For input string: \"asfasf\""))
    }
  }
}
