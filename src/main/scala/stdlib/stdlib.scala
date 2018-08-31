package stdlib

package object stdlib {

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)

    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] =
      F.chain(fa, afb)
  }

  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  def putStrLn[F[_] : Console](line: ConsoleOut): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)

}
