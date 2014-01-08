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
        parsePerson("", "") shouldEqual Bad(One("\"\" is not a valid name")) // My but aren't we in a monadic mood today!
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
     
        def orInstances2[B]: Traverse[({type l[g] = g Or B})#l] with Monad[({type l[g] = g Or B})#l] with Cozip[({type l[g] = g Or B})#l] with Plus[({type l[g] = g Or B})#l] = ???
        implicit def accumulatingOrApplicativeForString: Applicative[({type l[g] = g Or String})#l] = OrInstances.accumulatingOrApplicativeForSemigroup

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
     
        def orInstances2[B]: Traverse[({type l[g] = g Or B})#l] with Monad[({type l[g] = g Or B})#l] with Cozip[({type l[g] = g Or B})#l] with Plus[({type l[g] = g Or B})#l] = ???
        implicit def accumulatingOrApplicativeForString: Applicative[({type l[g] = g Or List[String]})#l] = OrInstances.accumulatingOrApplicativeForSemigroup

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
     
        def orInstances2[B]: Traverse[({type l[g] = g Or B})#l] with Monad[({type l[g] = g Or B})#l] with Cozip[({type l[g] = g Or B})#l] with Plus[({type l[g] = g Or B})#l] = ???
        implicit def accumulatingOrApplicativeForString[B: Semigroup]: Applicative[({type l[g] = g Or B})#l] = OrInstances.accumulatingOrApplicativeForSemigroup

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
}

