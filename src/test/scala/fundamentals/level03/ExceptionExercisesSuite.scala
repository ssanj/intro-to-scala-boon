package fundamentals.level03

import fundamentals.level03.ExceptionExercises._
import boon._
import syntax._

object ExceptionExercisesSuite extends SuiteLike("ExceptionExercisesTest") {

  private implicit val personBoonType = BoonType.defaults[Person]

  private val t1 = test("getName") {
    ->>(
        getName("Fred") =?= "Fred" | "should return valid name if supplied",

        getName("") =!=[EmptyNameException](_ =?= "provided name is empty" |
          "should throw an EmptyNameException if the name supplied is empty")
    )
  }

  private val t2 = test("getAge") {
    ->>(
        getAge("20") =?= 20 | "should return valid age if supplied",

        getAge("Fred") =!=[InvalidAgeValueException](_ =?= "provided age is invalid: Fred" |
          "should throw an InvalidAgeValueException if the age supplied is not an Int"),

        getAge("0") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 0" |
          "should throw an InvalidAgeRangeException if the age supplied is lower than range 1-120"),

        getAge("1") =?= 1 | "should accept an age of one",

        getAge("120")   =?= 120 | "should accept an age of a hundred and twenty",

        getAge("121") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 121" |
          "should throw an InvalidAgeRangeException if the age supplied is higher than range 1-120")
    )
  }

  private val t3 = test("createPerson") {
    ->>(
        createPerson("Fred", "32") =?= Person("Fred", 32) |
          "should return Person if supplied a valid name and age",

        createPerson("", "32") =!=[EmptyNameException](_ =?= "provided name is empty" |
          "should throw an EmptyNameException if the name supplied is empty"),

        createPerson("Fred", "ThirtyTwo") =!=[InvalidAgeValueException](_ =?= "provided age is invalid: ThirtyTwo" |
          "should throw an InvalidAgeValueException if the age supplied is not an Int"),

        createPerson("Fred", "150") =!=[InvalidAgeRangeException](_ =?= "provided age should be between 1-120: 150" |
          "should throw an InvalidAgeRangeException if the age supplied is not between 1 and 120")
    )
  }

  private val t4 = test("createValidPeople") {
    createValidPeople =?= List(Person("Tokyo", 30), Person("Berlin", 43)) |
      "should return a List of Person instances"
  }

  private val t5 = test("collectErrors") {

    def assertSameException(e1: Exception, e2: Exception): ContinueSyntax =
      e1.getClass == e2.getClass | s"${e1.getClass.getName} class" and
      e1.getMessage =?= e2.getMessage | s"${e1.getClass.getName} message"

    val expectedErrors = List(
      new InvalidAgeValueException("provided age is invalid: 5o"),
      new InvalidAgeRangeException("provided age should be between 1-120: 200"),
      new InvalidAgeRangeException("provided age should be between 1-120: 0"),
      new EmptyNameException("provided name is empty")
    )

    ->|>(
      collectErrors.size =?= expectedErrors.size | "have expected number of errors",

      % {
        val assertions = collectErrors.zip(expectedErrors).map {
          case (e1, e2) => assertSameException(e1, e2)
        }

        //we know collectErrors is not empty so we can call .head safely
        NonEmptySeq.nes(assertions.head, assertions.tail:_*)
      }
    )
  }

  override val tests = NonEmptySeq.nes(t1, t2, t3, t4, t5)
}
