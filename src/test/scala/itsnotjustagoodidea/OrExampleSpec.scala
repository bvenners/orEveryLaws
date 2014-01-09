package itsnotjustagoodidea // It's the law!

import scalaz._
import Scalaz._
import org.scalatest._
import org.scalautils._
import org.scalautils.One
import OrInstances._

class OrExampleSpec extends WordSpec with Matchers {

  case class Person(name: String, age: Int)

  "An Or" when {

    "accumulating (i.e., its Bad type is an Every)" should {

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
      "by default do short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(One("\"\" is not a valid name")) // My but aren't we in a monadic mood today!
      }
      "be able to accumulate with Scalaz's applicative syntax" in {

        implicit def personality[G, B] = AccumulatingOr.forEvery[G, B]

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
    }

    "its Bad type is String" should {

      def parseName(input: String): String Or ErrorMessage = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) Good(trimmed) else Bad(s""""${input}" is not a valid name""")
      }

      def parseAge(input: String): Int Or ErrorMessage = {
        try {
          val age = input.trim.toInt
          if (age >= 0) Good(age) else Bad(s""""${age}" is not a valid age""")
        }
        catch {
          case _: NumberFormatException => Bad(s""""${input}" is not a valid integer""")
        }
      }

      "by default do short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or ErrorMessage = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual Bad("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual Bad("\"\" is not a valid name")
      }

      "be able to accumulate with Scalaz's applicative syntax with the right stimulus" in {
     
        implicit def personality = AccumulatingOr.accumulatingOrApplicativeForSemigroup[String]

        def parsePerson(inputName: String, inputAge: String): Person Or ErrorMessage = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual Bad("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual Bad("\"\" is not a valid integer\"\" is not a valid name")
      }
    }
    "its Bad type is List[String]" should {

      def parseName(input: String): String Or List[ErrorMessage] = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) Good(trimmed) else Bad(List(s""""${input}" is not a valid name"""))
      }

      def parseAge(input: String): Int Or List[ErrorMessage] = {
        try {
          val age = input.trim.toInt
          if (age >= 0) Good(age) else Bad(List(s""""${age}" is not a valid age"""))
        }
        catch {
          case _: NumberFormatException => Bad(List(s""""${input}" is not a valid integer"""))
        }
      }

      "by default do short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid name"))
      }

      "be able to accumulate with Scalaz's applicative syntax with the right stimulus" in {
     
        implicit def personality = AccumulatingOr.accumulatingOrApplicativeForSemigroup[List[String]]

        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid integer", "\"\" is not a valid name"))
      }

      "be able to accumulate with Scalaz's applicative syntax as a Semigroup" in {
     
        implicit def personality[B: Semigroup] = AccumulatingOr.accumulatingOrApplicativeForSemigroup[B]

        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid integer", "\"\" is not a valid name"))
      }
    }
  }
  "An Every" when {
     "used to accumulate errors" can {
       "be transformed to any other desired type at the end of the day" in {
         val result = Good[Person].orBad(Many("\"\" is not a valid name", "\"\" is not a valid integer"))
         result.badMap(_.mkString) shouldEqual Bad("\"\" is not a valid name\"\" is not a valid integer")
         result.badMap(_.toList) shouldEqual Bad(List("\"\" is not a valid name","\"\" is not a valid integer"))  
       }
     }
   }
}

