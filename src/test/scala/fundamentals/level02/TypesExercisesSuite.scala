package fundamentals.level02

import fundamentals.level02.TypesExercises._
import boon._
import syntax._

object TypesExercisesSuite extends SuiteLike("TypesExercisesTest") {

  private implicit val personBoonType = BoonType.defaults[Person]
  private implicit val walletBoonType = BoonType.defaults[Wallet]

  override val tests = oneOrMore(
    test("showPerson1") {
      val person = Person("Bob", 50)
      showPerson1(person) =?= "Bob is 50 years old" | "should turn Bob into a String"
    },

    test("showPerson2") {
      val person = Person("Bob", 50)
      val updatedPerson = showPerson2(person)

      updatedPerson =?= "Bob is 50 years old" | "should turn Bob into a String"
    },

    test("changeName") {
      val person = Person("Bob", 50)
      val updatedPerson = changeName("Sarah", person)

      updatedPerson =?= Person("Sarah", 50) | "should update the Person's name" and
      person        =?= Person("Bob", 50)   | "Should copy person" // assertion is unnecessary, but shows immutability
    },

    test("showWallet") {
      val wallet = Wallet(23.4)
      val str = showWallet(wallet)

      str =?= "The wallet amount is 23.4" | "should show the wallet amount as a String"
    },

    test("purchase") {
      val wallet = Wallet(100)
      val updatedWallet = purchase(25, wallet)

      updatedWallet =?= Wallet(75)  | "should return a wallet with cost deducted" and
      wallet        =?= Wallet(100) | "should not mutate the original wallet"
    },

    test("showTrafficLightStr") {
      showTrafficLightStr("red")      =?= "The traffic light is red"      | "should show red"    and
      showTrafficLightStr("yellow")   =?= "The traffic light is yellow"   | "should show yellow" and
      showTrafficLightStr("green")    =?= "The traffic light is green"    | "should show green"  and
      showTrafficLightStr("purple")   =?= "invalid traffic light: purple" | "should return a default on other inputs" and
      showTrafficLightStr("flashing") =?= "The traffic light is flashing" | "should show flashing"
    },

    test("showTrafficLight") {
      oneOrMore(
        showTrafficLight(Red)      =?= "The traffic light is red"      | "should show Red",
        showTrafficLight(Yellow)   =?= "The traffic light is yellow"   | "should show Yellow",
        showTrafficLight(Green)    =?= "The traffic light is green"    | "should show Green",
        showTrafficLight(Flashing) =?= "The traffic light is flashing" | "should show Flashing"
      )
    }

  )
}
