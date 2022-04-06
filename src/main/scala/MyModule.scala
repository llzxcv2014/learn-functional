object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val message = "The absolute value of %d is %d"
    message.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val message = "The factorial of %d is %d"
    message.format(n, abs(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d id %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }
}
