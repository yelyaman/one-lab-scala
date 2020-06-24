package one.lab.tasks.week.one

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Operators._

class OperatorsTest extends AnyFunSuite with Matchers {

  test("getPersonInfo should work with custom arguments") {
    val name    = "Batyr"
    val surname = "Moldabekov"
    val age     = 33

    getPersonInfo(name, surname, age) shouldBe s"$name $surname $age"
  }

  test("getPersonInfoClass should work correct") {
    val name    = "Batyr"
    val surname = "Moldabekov"
    val age     = 33
    val person  = new Person(name, surname, age)

    getPersonInfoObject(person) shouldBe s"$name $surname $age"
  }
}
