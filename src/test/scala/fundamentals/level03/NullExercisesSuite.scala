package fundamentals.level03

import fundamentals.level03.NullExercises._
import boon._
import syntax.nulls._

object NullExercisesSuite extends SuiteLike("NullExercises") {

  private implicit val boonTrafficLight = BoonType.defaults[TrafficLight]
  private implicit val boonPerson       = BoonType.defaults[Person]

  override val tests = oneOrMore(
    test("mkTrafficLightOrNull") {
      null_?(mkTrafficLightOrNull("invalid"))(pass | "null given unknown String") and
      mkTrafficLightOrNull("red")    =?= Red       | "Red"                        and
      mkTrafficLightOrNull("yellow") =?= Yellow    | "Yellow"                     and
      mkTrafficLightOrNull("green")  =?= Green     | "Green"
    },

    test("mkTrafficLightOrNullThenShow") {
      mkTrafficLightOrNullThenShow("red")    =?= "Traffic light is red"     | "show Red"     and
      mkTrafficLightOrNullThenShow("yellow") =?= "Traffic light is yellow"  | "show Yellow"  and
      mkTrafficLightOrNullThenShow("green")  =?= "Traffic light is green"   | "show Green"   and
      mkTrafficLightOrNullThenShow("bob")    =?= "Traffic light is invalid" | "show invalid"
    },

    test("mkPersonOrNull") {
      mkPersonOrNull("Bob", 20) =?= Person("Bob", 20) | "should return Person"                 and
      null_?(mkPersonOrNull("", 20))(pass             | "should return null if name is blank") and
      null_?(mkPersonOrNull("Bob", -1))(pass          | "should return null if age < 0")
    },

    test("mkPersonOrNullThenChangeName") {
      oneOrMore(
        mkPersonOrNullThenChangeName("Old Name", 20, "New Name") =?= Person("New Name", 20) |
          "should return Person with name set to newName",

        mkPersonOrNullThenChangeName("", 20, "New Name")         =?= null                   |
          "should return null if oldName is blank",

        mkPersonOrNullThenChangeName("Bob", 20, "")              =?= null                   |
          "should return null if newName is blank",

        mkPersonOrNullThenChangeName("Old Name", -1, "New Name") =?= null                   |
          "should return null if age < 0"
      )
    }
  )
}
