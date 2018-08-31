package stdlib

trait Console[F[_]] {
  def putStrLn(line: ConsoleOut): F[Unit]

  def getStrLn: F[String]
}

object Console {
  def apply[F[_]](implicit F: Console[F]): Console[F] = F
}