import scala.io.StdIn.readLine
import scala.util.Try

/*
 * Step 4.
 * Add Console[IO].
 * */
/*
 This is an interim code. It does not compile.

object App2_4 {

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

  /*
   * we're going to need something a little different...
   * so we're going to need something like this our console which gives us the ability to do put your line and
   * you get star line and we'll add a little helper function down here get an implicit console of F if
   * that's in scope put stir line now actually can work with any console any F for which there's a console defined for
   * it and what it's going to do is take that line of employment return an F of unit it's going to do that by doing
   * console up get sir line is going to be looks exactly the same
   */

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]

    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  /*
    * so now I have the ability to describe using this type class something that can put string to the console and
    * something they can get string from the console so this is a bundle of capabilities and it can be implemented
    * for any F which is capable of providing something that's console like console like input and output
     */

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

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
    /*
     * these will be my helper methods here that I'll call directly and then down here I don't need these anymore
     * instead what I need to do is I need to fate and implicit about for for program here and it's going to be console IO
     * to implement these two methods here I've already done I just moved in there
     * */
    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      def putStrLn(line: String): IO[Unit] = IO(() => println(line))

      def getStrLn: IO[String] = IO(() => readLine())
    }

  }

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
*/