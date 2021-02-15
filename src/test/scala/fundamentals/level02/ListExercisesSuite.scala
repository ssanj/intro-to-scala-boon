package fundamentals.level02

import fundamentals.level02.ListExercises._
import fundamentals.level02.TypesExercises.{Person, showPerson1}
import boon._
import syntax._

object ListExercisesSuite extends SuiteLike("ListExercisesSuite") {

  private implicit val personBoonType = BoonType.defaults[Person]

  override def tests = oneOrMore(
    test("prepend") {
      prependToList(1, List(2, 3, 4)) =?= List(1, 2, 3, 4) | "should add an element to the start of the List"
    },

    test("append") {
      appendToList(1, List(2, 3, 4)) =?= List(2, 3, 4, 1) | "should add an element to the end of the List"
    },

    test("empty") {
      isEmptyList(Nil)            | "should return True for Nil" and
      !isEmptyList(1 :: 2 :: Nil) | "should return False for non-empty List"
    },

    test("size") {
      showListSize(Nil) =?= "This is an empty list"                   | "should show size for Nil" and
      showListSize(1 :: 2 :: 3 :: Nil) =?= "This is a list of size 3" | "should show size for non-empty List"
    },

    test("map") {
      addNumToEach(5, Nil) =?= Nil                     | "should return Nil given Nil" and
      addNumToEach(5, List(1, 2, 3)) =?= List(6, 7, 8) | "should add 5 to each element of non-empty List"
    },

    test("filter") {
      filterEven(List(1, 2, 3, 4, 5, 6)) == List(2, 4, 6) | "should return even numbers"
    },

    test("product") {
      product(Nil) =?= 1            | "should return 1 given Nil" and
      product(List(2, 5, 3)) =?= 30 | "should multiply all the elements of non-empty List"
    },

    test("min") {
      min(Nil) =?= Int.MinValue | "should return smallest Int given Nil" and
      min(List(4, 6, 1)) =?= 1  | "should return smallest number in non-empty List"
    },

    test("youngest") {
      youngestPerson(Nil) =?= Person("Nobody", 0) | "should return a silly default person given Nil" and
      %@(Person("Karen Page", 27)){ youngest =>
        youngestPerson(peopleList) =?= youngest   |
          "should return the first person in the list with the smallest age given a non-empty List"
      }
    },

    test("personWithIndex") {
      personWithIndex(peopleList) =?= (
        List((Person("Matt Murdock",            30), 1),
             (Person("Karen Page",              27), 2),
             (Person("Franklin 'Foggy' Nelson", 31), 3),
             (Person("Claire Temple",           32), 4),
             (Person("Wilson Fisk",             42), 5),
             (Person("Elektra Natchios",        27), 6))
      ) | "should return each person with their 1-based index"
    },

    test("showEveryNthPerson") {
      val people = ListExercises.peopleList
      val all = people.map(showPerson1)
      showEveryNthPerson(-5, people) =?= all                                      | "negative index" and
      showEveryNthPerson(0, people)  =?= all                                      | "index zero"     and
      showEveryNthPerson(1, people)  =?= all                                      | "index one"      and
      showEveryNthPerson(2, people)  =?= (
        List(
              "Karen Page is 27 years old",
              "Claire Temple is 32 years old",
              "Elektra Natchios is 27 years old"
        ))                                                                        | "index two"      and
      showEveryNthPerson(3, people)  =?= (
        List(
              "Franklin 'Foggy' Nelson is 31 years old",
              "Elektra Natchios is 27 years old"
        ))                                                                        | "index three"    and
      showEveryNthPerson(5, people)  =?= List("Wilson Fisk is 42 years old")      |  "index four"    and
      showEveryNthPerson(6, people)  =?= List("Elektra Natchios is 27 years old") | "index five"     and
      showEveryNthPerson(8, people)  =?= Nil                                      | "index six"
    },

    test("sublists") {
      val inputList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
      val expected  = List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e"))
      val result = sublists(inputList)
      result =?= expected | "sublists match"
    },

    test("consecutive duplicates") {
      val inputList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
      val result = sublists(inputList)
      val expected = List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e"))
      result =?= expected | "should pack consecutive duplicates of list elements into sublists"
    },

    test("getNames") {
      val powerRangers = List(Person("Red Ranger", 22), Person("Yellow Ranger", 20), Person("Pink Ranger", 21))
      val names = getNames(powerRangers)
      names =?= List("Red Ranger", "Yellow Ranger", "Pink Ranger") | "should return the names of all persons"
    },

    test("getAdults") {
      val powerRangers = List(Person("Red Ranger", 17), Person("Yellow Ranger", 18), Person("Pink Ranger", 19))
      val adults = getAdults(powerRangers)
      adults =?= List(Person("Yellow Ranger", 18), Person("Pink Ranger", 19)) | "should return persons aged >= 18"
    },

    test("reverseList") {
      val inputList = List(1, 2, 3)
      val result = reverseList(inputList)
      result =?= List(3, 2, 1) | "should return the input list reversed"
    }
  )
}
