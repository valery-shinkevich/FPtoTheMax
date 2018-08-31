import stdlib.{Console, ConsoleOut, Program, Random}

object TestIO {

  def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))

  implicit val ProgramTestIO: Program[TestIO] = new Program[TestIO] {
    override def finish[A](a: => A): TestIO[A] = TestIO.point(a)

    override def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] =
      fa.flatMap(afb)

    override def map[A, B](fa: TestIO[A], ab: A => B): TestIO[B] = fa.map(ab)
  }

  implicit val ConsoleTestIO: Console[TestIO] = new Console[TestIO] {
    override def putStrLn(line: ConsoleOut): TestIO[Unit] = TestIO(t => t.putStrLn(line.en))

    override def getStrLn: TestIO[String] = TestIO(t => t.getStrLn)
  }

  implicit val RandomTestIO: Random[TestIO] = new Random[TestIO] {
    override def nextInt(upper: Int): TestIO[Int] = TestIO(t => t.nextInt(upper))
  }

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