package one.lab.tasks.week.one

import one.lab.tasks.week.one.Interfaces._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterfacesTest extends AnyFunSuite with Matchers {
  test("Console should suitable with game's console type") {
    val playStation = new PlayStation
    val xbox        = new Xbox
    val sega        = new Sega

    val forza   = new ForzaHorizon
    val witcher = new TheWitcher
    val mario   = new SuperMario

    xbox.play(forza) shouldBe s"playing ${forza.game}"
    playStation.play(forza) shouldBe "disk format is invalid"
    sega.play(forza) shouldBe "disk format is invalid"

    xbox.play(witcher) shouldBe "disk format is invalid"
    playStation.play(witcher) shouldBe s"playing ${witcher.game}"
    sega.play(witcher) shouldBe "disk format is invalid"

    xbox.play(mario) shouldBe "disk format is invalid"
    playStation.play(mario) shouldBe "disk format is invalid"
    sega.play(mario) shouldBe s"playing ${mario.game}"
  }
}
