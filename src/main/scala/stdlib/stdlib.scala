package stdlib

package object stdlib {

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }

  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  def putStrLn[F[_] : Console](line: ConsoleOut): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)

  object implicits {

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      override def finish[A](a: => A): IO[A] = IO.point(a)

      override def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      override def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      override def putStrLn(line: ConsoleOut): IO[Unit] = IO(() => println(line.en))

      override def getStrLn: IO[String] = IO(() => readLine())
    }

    implicit val RandomIO: Random[IO] = (upper: Int) => IO(() => scala.util.Random.nextInt(upper))
  }

}
