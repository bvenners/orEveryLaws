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
package itsnotjustagoodidea // It's the law!

import scalaz._
import Scalaz._
import org.scalatest._
import org.scalautils._
import org.scalautils.One
import OrInstances._

//
// This test suite demonstrates ScalaUtils' Or acting alternatively as a
// monad and an accumulating applicative, both via its built-in syntax
// and throgh Scalaz's Monad and Applicative typeclasses. We demonstrate it
// short-circuiting (like a monad) and accumulating with Every, NonEmptyList,
// String, and List[String]. It's behavior defaults to monadic, but its
// latent accumulating applicative personality can be broght to the fore 
// with a one-line implicit definition.
// 
class OrExampleSpec extends UnitSpec {

  case class Person(name: String, age: Int)

  "An Or" when {

    "its Bad type is an Every" should {

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

        // import ScalaUtils' accumulation API (doesn't require applicatives)
        import Accumulation._
  
        def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          withGood(name, age) { Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(Many("\"\" is not a valid name", "\"\" is not a valid integer")) // accumulated here
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) } // Works because we imported OrInstances._ above
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(One("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {

        // Hide the monadic personality and establish the accumulating personality with one line
        implicit def personality[G, B] = AccumulatingOr.applicativeForEvery[G, B]

        def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(Many("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }
    }

    "its Bad type is NonEmptyList[String]" should {

      def parseName(input: String): String Or NonEmptyList[String] = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) Good(trimmed) else Bad(NonEmptyList(s""""${input}" is not a valid name"""))
      }

      def parseAge(input: String): Int Or NonEmptyList[String] = {
        try {
          val age = input.trim.toInt
          if (age >= 0) Good(age) else Bad(NonEmptyList(s""""${age}" is not a valid age"""))
        }
        catch {
          case _: NumberFormatException => Bad(NonEmptyList(s""""${input}" is not a valid integer"""))
        }
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or NonEmptyList[String] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(NonEmptyList("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(NonEmptyList("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(NonEmptyList("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {
     
        // Hide the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingOr.applicativeFor[NonEmptyList[String]]

        def parsePerson(inputName: String, inputAge: String): Person Or NonEmptyList[String] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(NonEmptyList("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(NonEmptyList("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(NonEmptyList("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
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

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or ErrorMessage = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual Bad("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual Bad("\"\" is not a valid name") // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {
     
        // Hide the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingOr.applicativeFor[String]

        def parsePerson(inputName: String, inputAge: String): Person Or ErrorMessage = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual Bad("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual Bad("\"\" is not a valid integer\"\" is not a valid name") // accumulated here
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

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {
     
        // Hide the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingOr.applicativeFor[List[String]]

        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }

      "be able to accumulate with Scalaz's applicative syntax as a Semigroup via an alternate personality" in {
     
        // Hide the monadic personality and establish the accumulating personality with one line
        // This shows you can accumulate any Semigroup with one import, like the behavior of Scalaz's Validation
        // I actually think this is a bad idea, though, because it is less obvious what types will accumulate.
        // Better to just turn on the accumulating personality for the specific type into which you want to
        // accumulate, as I did in the other examples, but this example shows it is possible to
        // be more general.
        implicit def personality[B: Semigroup] = AccumulatingOr.applicativeFor[B]

        def parsePerson(inputName: String, inputAge: String): Person Or List[ErrorMessage] = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual Good(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual Bad(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual Bad(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual Bad(List("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }
    }
  }

  // This demonstrates that the other forms we accumulated into above through applicative can be produced
  // using ScalaUtils' built-in accumulation behavior by accumulating into Every, then "bad mapping" the result
  // into the desired type.
  "An Every" when {
     "used to accumulate errors" can {
       "be transformed to any other desired type at the end of the day" in {
         val result = Good[Person].orBad(Every("\"\" is not a valid name", "\"\" is not a valid integer"))
         result.badMap(every => NonEmptyList(every.head, every.tail: _*)) shouldEqual Bad(NonEmptyList("\"\" is not a valid name", "\"\" is not a valid integer"))
         result.badMap(_.mkString) shouldEqual Bad("\"\" is not a valid name\"\" is not a valid integer")
         result.badMap(_.toList) shouldEqual Bad(List("\"\" is not a valid name","\"\" is not a valid integer"))  
       }
     }
   }
}

