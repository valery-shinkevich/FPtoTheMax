

case class TestData(input: Vector[String], output: Vector[String], nums: Vector[Int]) {

  def putStrLn(line: String): (TestData, Unit) = (copy(output = output :+ line), ())

  def getStrLn: (TestData, String) = (copy(input = input.tail), input.head)

  def nextInt(upper: Int): (TestData, Int) = (copy(nums = nums.tail), nums.head)

  def showResults = output.reverse.mkString("\n")
}





