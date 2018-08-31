import scala.io.StdIn.readLine
import scala.util.Try

/*
 * Step 5.
 * Add Random[F[_]].
 * */
/*
 This is an interim code. It does not compile.

object App2_5 {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]

    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] =
      F.chain(fa, afb)
  }

  def point[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]

    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

  /*
  * in order to complete the refactoring that's this thing here I'll just call that random and you know trait
  * called random that one capability and then I need to create a function and use this function of the top-level
  * */
  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }

  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)

  case class IO[A](unsafeRun: () => A) {
    self =>

    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      override def finish[A](a: => A): IO[A] = IO.point(a)

      override def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] =
        fa.flatMap(afb)

      override def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      override def putStrLn(line: String): IO[Unit] = IO(() => println(line))

      override def getStrLn: IO[String] = IO(() => readLine())
    }

    /*
    * finally I need to create an implementation of this for i/o
    * */
    implicit val RandomIO: Random[IO] = new Random[IO] {
      override def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }

  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

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
*/