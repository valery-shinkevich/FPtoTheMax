import scala.util.Try

/*
 * Step 1.
 * Add user input checking.
 * */
object App2_1 {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main(args: Array[String]): Unit = {

    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    var exec = true
    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1
      println("Dear " + name + ", please guess a number from 1 ro 5:")

      parseInt(readLine) match {
        case None => println("You did not enter a number!")
        case Some(guess) =>
          if (guess == num) println("You guessed right, " + name + "!")
          else
            println("You guessed wrong, " + name + "! The number was: " + num)
      }

      var repeat = true
      while (repeat) {
        repeat = false

        println("Do you want to repeat, " + name + "?")

        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _ => repeat = true
        }
      }
    }
  }
}
