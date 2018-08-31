import stdlib.ConsoleOut._
import stdlib._
import scala.util.Try

/*
 * Step 1.
 * Add case classes for output messages then refactoring code.
 * */
object App3 {

  import stdlib._

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def askForRepeat[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _ <- putStrLn(DoYouWantToRepeat(name))
      input <- getStrLn.map(_.toLowerCase)
      repeat <- input match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _ => putStrLn(ThatIsNotValid(name)); askForRepeat(name)
      }
    } yield repeat

  def gameLoop[F[_] : Program : Random : Console](name: String): F[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn(PleaseGuess(name))
      input <- getStrLn
      _ <- printResults(name, num, input)
      repeat <- askForRepeat(name)
      _ <- if (repeat) gameLoop(name) else finish(())

    } yield ()

  private def printResults[F[_] : Program : Random : Console](name: String, num: Int, input: String): F[Unit] = {
    parseInt(input)
      .fold[F[Unit]](putStrLn(WrongNumber()))(
      guess =>
        if (guess == num) putStrLn(YouGuessedRight(name))
        else putStrLn(YouGuessedWrong(name, num))
    )
  }

  def main[F[_] : Program : Random : Console]: F[Unit] = {
    for {
      _ <- putStrLn(WhatIsYourName())
      name <- getStrLn
      _ <- putStrLn(WelcomeToGame(name))
      _ <- gameLoop(name)
    } yield ()
  }

  def mainIO: IO[Unit] = main[IO]

}
