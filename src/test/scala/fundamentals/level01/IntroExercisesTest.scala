package fundamentals.level01

import fundamentals.level01.IntroExercises._
import boon._

object IntroExercisesTest extends SuiteLike("IntroExerciseTest") {

  val t1 = test("add") {
    add(1, 2) =?= 3  | "1 + 2 is 3" and
    add(5, 7) =?= 12 | "5 + 7 is 12"
  }

  val t2 = test("addCurried") {
    addCurried(1)(2) =?= 3  | "1 + 2 is 3" and
    addCurried(5)(7) =?= 12 | "5 + 7 is 12"
  }

  val t3 = test("add5") {
    add5(4) =?= 9   | "5 + 4 is 9" and
    add5(10) =?= 15 | "10 + 5 is 15"
  }

  val t4 = test("puzzlers") {
    foo(1) =?= 1             | "foo is the'identity' function" and
    bar(1).isInstanceOf[Int] | "bar can be any Int"
  }

  val t5 = test("timesTwoIfEven") {
    timesTwoIfEven(2) =?= 4 | "given 2 returns 4" and
    timesTwoIfEven(3) =?= 3 | "given 3 returns 3"
  }

  val t6 = test("showNumber") {
    showNumber(1) =?= "The number is 1" | "should string interpolate"
  }

  val t7 = test("pair") {
    pair("Jimmy", 25) =?= ("Jimmy", 25) | "construct Jimmy with age" and
    pair("Sammy", 30) =?= ("Sammy", 30) | "construct Sammy with age" and
    fst("Jimmy", 25) =?= "Jimmy"        | "fst with Jimmy"           and
    fst("Sammy", 30) =?= "Sammy"        | "fst with Sammy"           and
    snd("Jimmy", 25) =?= 25             | "snd with Jimmy"           and
    snd("Sammy", 30) =?= 30             | "snd with Sammy"
  }

  val tests = oneOrMore(t1, t2, t3, t4, t5, t6, t7)
}
