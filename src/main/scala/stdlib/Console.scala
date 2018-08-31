package stdlib

trait Console[F[_]] {
  def putStrLn(line: ConsoleOut): F[Unit]

  def getStrLn: F[String]
}

object Console {
  def apply[F[_]](implicit F: Console[F]): Console[F] = F

  implicit val ConsoleIO: Console[IO] = new Console[IO] {
    override def putStrLn(line: ConsoleOut): IO[Unit] = IO(() => println(line.en))

    override def getStrLn: IO[String] = IO(() => readLine())
  }
}