package fundamentals.level02

import fundamentals.level02.TypesExercises._
import boon._
import syntax._

object TypesExercisesTest extends SuiteLike("TypesExercisesTest") {

  val t1 = test("showPerson1") {
    val person = Person("Bob", 50)
    showPerson1(person) =?= "Bob is 50 years old" | "should turn Bob into a String"
  }

  val t2 = test("showPerson2") {
    val person = Person("Bob", 50)
    val updatedPerson = defer(showPerson2(person))
    updatedPerson =?= "Bob is 50 years old" | "should turn Bob into a String"
  }

  val t3 = test("changeName") {
    val person = Person("Bob", 50)
    val updatedPerson = defer(changeName("Sarah", person))

    (updatedPerson =?= Person("Sarah", 50) | "should update the Person's name") &
    (person =?= Person("Bob", 50) | "Should copy person") // assertion is unnecessary, but shows immutability
  }

  val t4 = test("showWallet") {
    val wallet = Wallet(23.4)
    val str = defer(showWallet(wallet))
    str =?= "The wallet amount is 23.4" | "should show the wallet amount as a String"
  }

  val t5 = test("purchase") {
    val wallet = Wallet(100)
    val updatedWallet = defer(purchase(25, wallet))
    (updatedWallet =?= Wallet(75) | "should return a wallet with cost deducted") &
    (wallet =?= Wallet(100) | "should not mutate the original wallet")
  }

  val t6 = test("showTrafficLightStr") {
    {
      val str = defer(showTrafficLightStr("red"))
      str =?= "The traffic light is red" | "should show red"
    } &
    {
      val str = defer(showTrafficLightStr("yellow"))
      str =?= "The traffic light is yellow" | "should show yellow"
    } &
    {
      val str = defer(showTrafficLightStr("green"))
      str =?= "The traffic light is green" | "should show green"
    } &
    {
      val str = defer(showTrafficLightStr("purple"))
      str =?= "invalid traffic light: purple" | "should return a default on other inputs"
    } &
    {
      val str = defer(showTrafficLightStr("flashing"))
      str  =?= "The traffic light is flashing" | "should show flashing"
    }
  }

  // describe("showTrafficLight") {

  //   it("should show Red") {
  //     val str = showTrafficLight(Red)

  //     assert(str === "The traffic light is red")
  //   }

  //   it("should show Yellow") {
  //     val str = showTrafficLight(Yellow)

  //     assert(str === "The traffic light is yellow")
  //   }

  //   it("should show Green") {
  //     val str = showTrafficLight(Green)

  //     assert(str === "The traffic light is green")
  //   }

  //   it("should show Flashing") {
  //     ???
  //   }

  // }

  val tests = NonEmptySeq.nes(t1, t2, t3, t4, t5, t6)
}
