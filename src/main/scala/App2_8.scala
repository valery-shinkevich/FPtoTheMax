import scala.io.StdIn.readLine
import scala.util.Try

/*
 * Step 8.
 * refactoring
 * */
object App2_8 {

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

  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

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

    implicit val RandomIO: Random[IO] = new Random[IO] {
      override def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }

  }

  case class TestData(input: List[String], output: List[String], nums: List[Int]) {

    def putStrLn(line: String): (TestData, Unit) = (copy(output = line :: output), ())

    def getStrLn: (TestData, String) = (copy(input = input.tail), input.head)

    def nextInt(upper: Int): (TestData, Int) = (copy(nums = nums.tail), nums.head)

    def showResults = output.reverse.mkString("\n")
  }

  case class TestIO[A](run: TestData => (TestData, A)) {
    self =>
    def map[B](ab: A => B): TestIO[B] =
      TestIO(t => self.run(t) match {
        case (t, a) => (t, ab(a))
      })

    def flatMap[B](afb: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match {
        case (t, a) => afb(a).run(t)
      })

    def eval(t: TestData): TestData = run(t)._1
  }

  object TestIO {

    def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    implicit val ProgramTestIO: Program[TestIO] = new Program[TestIO] {
      override def finish[A](a: => A): TestIO[A] = TestIO.point(a)

      override def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] =
        fa.flatMap(afb)

      override def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
    }

    implicit val ConsoleTestIO: Console[TestIO] = new Console[TestIO] {
      override def putStrLn(line: String): TestIO[Unit] = TestIO(t => t.putStrLn(line))

      override def getStrLn: TestIO[String] = TestIO(t => t.getStrLn)
    }

    implicit val RandomTestIO: Random[TestIO] = new Random[TestIO] {
      override def nextInt(upper: Int): TestIO[Int] = TestIO(t => t.nextInt(upper))
    }

  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def askForRepeat[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _ <- putStrLn("Do you want to repeat, " + name + "?")
      input <- getStrLn.map(_.toLowerCase)
      repeat <- input match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _ => askForRepeat(name)
      }
    } yield repeat

  def gameLoop[F[_] : Program : Random : Console](name: String): F[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 ro 5:")
      input <- getStrLn
      _ <- printResults(name, num, input)
      repeat <- askForRepeat(name)
      _ <- if (repeat) gameLoop(name) else finish(())

    } yield ()

  private def printResults[F[_] : Program : Random : Console](name: String, num: Int, input: String): F[Unit] = {
    parseInt(input)
      .fold[F[Unit]](putStrLn("You did not enter a number!"))(
      guess =>
        if (guess == num) putStrLn("You guessed right, " + name + "!")
        else
          putStrLn("You guessed wrong, " + name + "! The number was: " + num)
    )
  }

  def main[F[_] : Program : Random : Console]: F[Unit] = {
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample =
    TestData(
      input = "John" :: "1" :: "n" :: Nil,
      nums = 0 :: Nil,
      output = Nil

    )

  def testRun = mainTestIO.eval(TestExample).showResults
}
