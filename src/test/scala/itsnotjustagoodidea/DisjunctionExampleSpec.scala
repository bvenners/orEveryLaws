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

//
// This test suite demonstrates Scalaz's \/ acting alternatively as a
// monad and an accumulating applicative. We demonstrate it
// short-circuiting (like a monad) and accumulating with Every, NonEmptyList,
// String, and List[String]. It's behavior defaults to monadic, but its
// latent accumulating applicative personality can be brought to the fore 
// with a one-line implicit definition.
// 
class DisjunctionExampleSpec extends UnitSpec {

  case class Person(name: String, age: Int)

  "A \\/" when {

    "its left type is a ScalaUtils' Every" should {

      def parseName(input: String): One[ErrorMessage] \/ String = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) \/-(trimmed) else -\/(One(s""""${input}" is not a valid name"""))
      }

      def parseAge(input: String): One[ErrorMessage] \/ Int = {
        try {
          val age = input.trim.toInt
          if (age >= 0) \/-(age) else -\/(One(s""""${age}" is not a valid age"""))
        }
        catch {
          case _: NumberFormatException => -\/(One(s""""${input}" is not a valid integer"""))
        }
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {

        def parsePerson(inputName: String, inputAge: String): Every[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
        // I currently don't know why the above line of code works, but it does. Scalaz has some
        // magic going on that produces a Monad instance for Every. My best guess is that because
        // ScalaUtils has an implicit conversion from Every to IndexedSeq, Scalaz somehow
        // ties into that. Be interested to find out what the mechanism is.
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(One("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {

        // Overpower the monadic personality, which I can't explain, and establish the accumulating
        // personality with one line The reason this "overpowers" the monadic one, is that the monadic
        // personality is almost surely being provided in the \/ companion object. Because the compiler
        // finds this one, it won't look in the companion object.
        implicit def personality[G, B] = AccumulatingDisjunction.applicativeForEvery[G, B]

        // This is needed to widen the left type from One[T] to Every[T], because otherwise
        // it doesn't work with Scalaz's applicative builder. (I don't fully understand why yet.)
        implicit class LeftWidener[G, B, EVERY[b] <: Every[b]](or: EVERY[B] \/ G) {
          def toAccEvery: Every[B] \/ G = or
        }

        def parsePerson(inputName: String, inputAge: String): Every[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccEvery |@| age.toAccEvery){ Person(_, _) }
        }

        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(One("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(One("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(Many("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }
    }

    "its left type is NonEmptyList[String]" should {

      def parseName(input: String): ErrorMessage \/ String = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) \/-(trimmed) else -\/(s""""${input}" is not a valid name""")
      }

      def parseAge(input: String): ErrorMessage \/ Int = {
        try {
          val age = input.trim.toInt
          if (age >= 0) \/-(age) else -\/(s""""${age}" is not a valid age""")
        }
        catch {
          case _: NumberFormatException => -\/(s""""${input}" is not a valid integer""")
        }
      }

      // This is needed to lift the left type from T to NonEmptyList[T]
      implicit class LeftWidener[G, B](disjunction: B \/ G) {
        def toAccNel: NonEmptyList[B] \/ G = disjunction.leftMap(NonEmptyList(_))
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
 
        def parsePerson(inputName: String, inputAge: String): NonEmptyList[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccNel |@| age.toAccNel){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(NonEmptyList("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(NonEmptyList("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(NonEmptyList("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {

        // Overpower the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingDisjunction.applicativeFor[NonEmptyList[String]]

        def parsePerson(inputName: String, inputAge: String): NonEmptyList[String] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccNel |@| age.toAccNel){ Person(_, _) }
        }

        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(NonEmptyList("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(NonEmptyList("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(NonEmptyList("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }
    }

    "its left type is String" should {

      def parseName(input: String): ErrorMessage \/ String = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) \/-(trimmed) else -\/(s""""${input}" is not a valid name""")
      }

      def parseAge(input: String): ErrorMessage \/ Int = {
        try {
          val age = input.trim.toInt
          if (age >= 0) \/-(age) else -\/(s""""${age}" is not a valid age""")
        }
        catch {
          case _: NumberFormatException => -\/(s""""${input}" is not a valid integer""")
        }
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): ErrorMessage \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual -\/("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual -\/("\"\" is not a valid name") // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {
     
        // Overpower the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingDisjunction.applicativeFor[String]

        def parsePerson(inputName: String, inputAge: String): ErrorMessage \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name |@| age){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/("\"\" is not a valid integer")
        parsePerson("Bridget Jones", "-29") shouldEqual -\/("\"-29\" is not a valid age")
        parsePerson("", "") shouldEqual -\/("\"\" is not a valid integer\"\" is not a valid name") // accumulated here
      }
    }
    "its left type is List[String]" should {

      def parseName(input: String): ErrorMessage \/ String = {
        val trimmed = input.trim
        if (!trimmed.isEmpty) \/-(trimmed) else -\/(s""""${input}" is not a valid name""")
      }

      def parseAge(input: String): ErrorMessage \/ Int = {
        try {
          val age = input.trim.toInt
          if (age >= 0) \/-(age) else -\/(s""""${age}" is not a valid age""")
        }
        catch {
          case _: NumberFormatException => -\/(s""""${input}" is not a valid integer""")
        }
      }

      // This is needed to lift the left type from T to List[T]
      implicit class LeftLifter[G, B](disjunction: B \/ G) {
        def toAccList: List[B] \/ G = disjunction.leftMap(List(_))
      }

      "by default exhibit short-circuting, monad-like behavior with Scalaz's applicative syntax" in {
     
        def parsePerson(inputName: String, inputAge: String): List[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccList |@| age.toAccList){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(List("\"\" is not a valid name")) // Short circuited here
      }

      "be able to accumulate with Scalaz's applicative syntax via an alternate personality" in {
     
        // Overpower the monadic personality and establish the accumulating personality with one line
        implicit def personality = AccumulatingDisjunction.applicativeFor[List[String]]

        def parsePerson(inputName: String, inputAge: String): List[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccList |@| age.toAccList){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(List("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }

      "be able to accumulate with Scalaz's applicative syntax as a Semigroup via an alternate personality" in {
     
        // Overpower the monadic personality and establish the accumulating personality with one line
        // This shows you can accumulate any Semigroup with one import, like the behavior of Scalaz's Validation
        // I actually think this is a bad idea, though, because it is less obvious what types will accumulate.
        // Better to just turn on the accumulating personality for the specific type into which you want to
        // accumulate, as I did in the other examples, but this example shows it is possible to
        // be more general.
        implicit def personality[B: Semigroup] = AccumulatingDisjunction.applicativeFor[B]

        def parsePerson(inputName: String, inputAge: String): List[ErrorMessage] \/ Person = {
          val name = parseName(inputName)
          val age = parseAge(inputAge)
          (name.toAccList |@| age.toAccList){ Person(_, _) }
        }
  
        parsePerson("Bridget Jones", "29") shouldEqual \/-(Person("Bridget Jones",29))
        parsePerson("Bridget Jones", "") shouldEqual -\/(List("\"\" is not a valid integer"))
        parsePerson("Bridget Jones", "-29") shouldEqual -\/(List("\"-29\" is not a valid age"))
        parsePerson("", "") shouldEqual -\/(List("\"\" is not a valid integer", "\"\" is not a valid name")) // accumulated here
      }
    }
  }
}

