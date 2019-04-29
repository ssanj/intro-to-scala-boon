package fundamentals.level03

import fundamentals.level03.NullExercises._
import boon._
import syntax.nulls._

object NullExercisesSuite extends SuiteLike("NullExercises") {

  private implicit val boonTrafficLight = BoonType.defaults[TrafficLight]

  private val t1 = test("mkTrafficLightOrNull") {
    null_?(mkTrafficLightOrNull("invalid"))(pass | "null given unknown String") and
    mkTrafficLightOrNull("red")    =?= Red       | "Red"                        and
    mkTrafficLightOrNull("yellow") =?= Yellow    | "Yellow"                     and
    mkTrafficLightOrNull("green")  =?= Green     | "Green"
  }

  private val t2 = test("mkTrafficLightOrNullThenShow") {
    mkTrafficLightOrNullThenShow("red")    =?= "Traffic light is red"     | "show Red"     and
    mkTrafficLightOrNullThenShow("yellow") =?= "Traffic light is yellow"  | "show Yellow"  and
    mkTrafficLightOrNullThenShow("green")  =?= "Traffic light is green"   | "show Green"   and
    mkTrafficLightOrNullThenShow("bob")    =?= "Traffic light is invalid" | "show invalid"
  }

  // describe("mkPersonOrNull") {

  //   it("should return Person") {
  //     assert(mkPersonOrNull("Bob", 20) === Person("Bob", 20))
  //   }

  //   it("should return null if name is blank") {
  //     assert(mkPersonOrNull("", 20) === null)
  //   }

  //   it("should return null if age < 0") {
  //     assert(mkPersonOrNull("Bob", -1) === null)
  //   }

  // }

  // describe("mkPersonOrNullThenChangeName") {

  //   it("should return Person with name set to newName") {
  //     assert(mkPersonOrNullThenChangeName("Old Name", 20, "New Name") === Person("New Name", 20))
  //   }

  //   it("should return null if oldName is blank") {
  //     assert(mkPersonOrNullThenChangeName("", 20, "New Name") === null)
  //   }

  //   it("should return null if newName is blank") {
  //     assert(mkPersonOrNullThenChangeName("Bob", 20, "") === null)
  //   }

  //   it("should return null if age < 0") {
  //     assert(mkPersonOrNullThenChangeName("Old Name", -1, "New Name") === null)
  //   }

  // }

  override val tests = oneOrMore(t1, t2)
}
