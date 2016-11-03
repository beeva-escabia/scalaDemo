object exercise21{
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, fib1: Int, fib2: Int): Int =
      if (n <= fib2) fib2
      else go(n, fib2, fib1 + fib2)
    go(n, 0, 1)
  }
  fibonacci(6)
}