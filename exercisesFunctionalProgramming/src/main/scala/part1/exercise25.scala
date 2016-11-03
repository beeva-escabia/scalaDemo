package part1

object exercise25{
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}