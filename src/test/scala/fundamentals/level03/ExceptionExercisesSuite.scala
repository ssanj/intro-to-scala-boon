package fundamentals.level03

import fundamentals.level03.ExceptionExercises._
import boon._
import model.AssertionData
import syntax.exception._
import syntax.equal._
import syntax.collection._

object ExceptionExercisesSuite extends SuiteLike("ExceptionExercisesTest") {

  private implicit val personBoonType = BoonType.defaults[Person]

  private val t1 = test("getName") {
    getName("Fred") =?= "Fred" | "should return valid name if supplied" and
    getName("") =!=[EmptyNameException](_ =?= "provided name is empty" |
      "should throw an EmptyNameException if the name supplied is empty")
  }

  private val t2 = test("getAge") {
    getAge("20") =?= 20 | "should return valid age if supplied" and
    getAge("Fred") =!=[InvalidAgeValueException](_ =?= "provided age is invalid: Fred" |
      "should throw an InvalidAgeValueException if the age supplied is not an Int") and
    getAge("0") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 0" |
      "should throw an InvalidAgeRangeException if the age supplied is lower than range 1-120") and
    getAge("1") =?= 1     | "should accept an age of one" and
    getAge("120") =?= 120 | "should accept an age of a hundred and twenty" and
    getAge("121") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 121" |
      "should throw an InvalidAgeRangeException if the age supplied is higher than range 1-120")
  }

  private val t3 = test("createPerson") {
    createPerson("Fred", "32") =?= Person("Fred", 32) |
      "should return Person if supplied a valid name and age" and
    createPerson("", "32") =!=[EmptyNameException](_ =?= "provided name is empty" |
      "should throw an EmptyNameException if the name supplied is empty") and
    createPerson("Fred", "ThirtyTwo") =!=[InvalidAgeValueException](_ =?= "provided age is invalid: ThirtyTwo" |
      "should throw an InvalidAgeValueException if the age supplied is not an Int") and
    createPerson("Fred", "150") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 150" |
      "should throw an InvalidAgeRangeException if the age supplied is not between 1 and 120")
  }

  private val t4 = test("createValidPeople") {
    createValidPeople =?= List(Person("Tokyo", 30), Person("Berlin", 43)) |
      "should return a List of Person instances"
  }

  private val t5 = test("collectErrors") {

    pass | "some reason"

  //   val expectedErrors = oneOrMore[Throwable](
  //     new InvalidAgeValueException("provided age is invalid: 5o"),
  //     new InvalidAgeRangeException("provided age should be between 1-120: 200"),
  //     new InvalidAgeRangeException("provided age should be between 1-120: 0"),
  //     new EmptyNameException("provided name is empty")
  //   )

  //   implicit val diffThrowable = model.Difference.genericDifference[Throwable]

  //   positionalSeq[Throwable](collectErrors, "collectErrors")(
  //     stopOnFailure(expectedErrors.map(isSame(_: Throwable))))
  }

  override val tests = oneOrMore(t1, t2, t3, t4, t5)
}
