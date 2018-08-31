package stdlib

sealed trait ConsoleOut {
  def en: String
}

/*
* these messages are structured information. they're not strings they're data types and they capture all
* the things that you would expect and then when you go ahead and run a program like this you actually
* get structured information out of it so instead of getting strings you get a description of what it
* wants to print and this has a couple of different benefits first off it's very very easy
* */
object ConsoleOut {

  case class WhatIsYourName() extends ConsoleOut {
    def en = "What is your name?"
  }

  case class WelcomeToGame(name: String) extends ConsoleOut {
    def en = s"Hello, $name, welcome to the game!"
  }

  case class PleaseGuess(name: String) extends ConsoleOut {
    def en = s"Dear $name, please guess a number from 1 to 5:"
  }

  case class WrongNumber() extends ConsoleOut {
    def en = "You did not enter a number!"
  }

  case class YouGuessedRight(name: String) extends ConsoleOut {
    def en = s"You guessed right, $name!"
  }

  case class YouGuessedWrong(name: String, num: Int) extends ConsoleOut {
    def en = s"You guessed wrong, $name! The number was: $num"
  }

  case class DoYouWantToRepeat(name: String) extends ConsoleOut {
    def en = s"Do you want to repeat, $name? (y/n)"
  }

  case class ThatIsNotValid(name: String) extends ConsoleOut {
    def en = s"That is not valid selection, $name!"
  }

}