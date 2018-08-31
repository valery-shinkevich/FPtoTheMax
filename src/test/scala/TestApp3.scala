import App3.main
import org.scalatest.FunSuite
import stdlib.ConsoleOut._

class TestApp3 extends FunSuite {

  val testExample =
    TestData(
      input = Vector("John" , "1" , "n"),
      nums = Vector(1),
      output = Vector()
    )

  val testResult = Vector(
    WhatIsYourName().en,
    WelcomeToGame("John").en,
    PleaseGuess("John").en,
    YouGuessedRight("John").en,
    DoYouWantToRepeat("John").en
  )

  def mainTestIO: TestIO[Unit] = main[TestIO]

  test("Try right guess") {
    assert(mainTestIO.eval(testExample).output === testResult)
  }

}
