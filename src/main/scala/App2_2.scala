import scala.util.Try

/*
 * Step 2.
 * Add IO.
 * */
object App2_2 {

  case class IO[A](unsafeRun: () => A) {
    self =>

    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  def putStrLn(line: String): IO[Unit] = IO(() => println(line))

  def getStrLn: IO[String] = IO(() => readLine())

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))

  def askForRepeat(name: String): IO[Boolean] =
    for {
      _ <- putStrLn("Do you want to repeat, " + name + "?")
      input <- getStrLn.map(_.toLowerCase)
      repeat <- input match {
        case "y" => IO point true
        case "n" => IO point false
        case _ => askForRepeat(name)
      }
    } yield repeat

  def gameLoop(name: String): IO[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 ro 5:")
      input <- getStrLn
      _ <- parseInt(input)
        .fold[IO[Unit]](putStrLn("You did not enter a number!"))(
        guess =>
          if (guess == num) putStrLn("You guessed right, " + name + "!")
          else
            putStrLn(
              "You guessed wrong, " + name + "! The number was: " + num
            )
      )
      repeat <- askForRepeat(name)
      _ <- if (repeat) gameLoop(name) else IO.point(())
    } yield ()

  def main: IO[Unit] = {
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }
}
