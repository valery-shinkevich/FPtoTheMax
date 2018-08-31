import scala.util.Try

/*
 * Step 3.
 * Add Program[F[_]].
 * */
object App2_3 {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]

    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  /*
   * Now, if we have an instance of this side class this program type class in scope
   * then automatically any F of a value will be enriched with map and flatmap methods of course
   * that's not super relevant for i/o because it already has these things but once we
   * start programming generically once we generalize our program here to be able to work on things which are not
   * necessarily i/o we're gonna need this capability in order to take advantage of this nice syntax that Scala gives us
   * */

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }

  def point[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  case class IO[A](unsafeRun: () => A) {
    self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      override def finish[A](a: => A): IO[A] = IO.point(a)

      override def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      override def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

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
