package stdlib

trait Random[F[_]] {
  def nextInt(upper: Int): F[Int]
}

object Random {
  def apply[F[_]](implicit F: Random[F]): Random[F] = F

  implicit val RandomIO: Random[IO] = (upper: Int) => IO(() => scala.util.Random.nextInt(upper))
}