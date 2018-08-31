package stdlib

trait Program[F[_]] {
  def finish[A](a: => A): F[A]

  def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

  def map[A, B](fa: F[A], ab: A => B): F[B]
}

object Program {
  def apply[F[_]](implicit F: Program[F]): Program[F] = F
}