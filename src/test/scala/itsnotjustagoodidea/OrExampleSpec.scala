package itsnotjustagoodidea // It's the law!

import scalaz._
import Scalaz._
import org.scalatest._
import org.scalautils._
import org.scalautils.One

class OrExampleSpec extends WordSpec with Matchers with OrInstances {

  case class Person(name: String, age: Int)

  def parseName(input: String): String Or Every[ErrorMessage] = {
    val trimmed = input.trim
    if (!trimmed.isEmpty) Good(trimmed) else Bad(One(s""""${input}" is not a valid name"""))
  }

  def parseAge(input: String): Int Or Every[ErrorMessage] = {
    try {
      val age = input.trim.toInt
      if (age >= 0) Good(age) else Bad(One(s""""${age}" is not a valid age"""))
    }
    catch {
      case _: NumberFormatException => Bad(One(s""""${input}" is not a valid integer"""))
    }
  }

  "An Accumulating Or" should {
    "accumulate with built-in syntax" in {
      import Accumulation._

      def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
        val name = parseName(inputName)
        val age = parseAge(inputAge)
        withGood(name, age) { Person(_, _) }
      }

      parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
      parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
      parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
      parsePerson("", "") shouldEqual Bad(Many("\"\" is not a valid name", "\"\" is not a valid integer"))
    }
    "accumulate with Scalaz's applicative syntax" in {
      def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
        val name = parseName(inputName)
        val age = parseAge(inputAge)
        (name |@| age){ Person(_, _) }
      }

      parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
      parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
      parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
      parsePerson("", "") shouldEqual Bad(Many("\"\" is not a valid integer", "\"\" is not a valid name"))
    }
    "be able to do short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
   
      // Make it do monadic applicative by hiding the accumulating implicit
      def accumulatingOrApplicative[G, B]: Applicative[({type l[g] = g Or Every[B]})#l] = ???

      def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
        val name = parseName(inputName)
        val age = parseAge(inputAge)
        (name |@| age){ Person(_, _) }
      }

      parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
      parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
      parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
      parsePerson("", "") shouldEqual Bad(One("\"\" is not a valid name"))
    }
   }
}

