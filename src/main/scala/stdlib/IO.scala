package stdlib

final case class IO[A](unsafeRun: () => A) {
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
}