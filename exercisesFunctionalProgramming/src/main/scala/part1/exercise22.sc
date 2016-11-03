object exercise22 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n+1 >= as.length) true
      else if (!ordered(as(n),as(n+1))) false
      else loop(n + 1)
    loop(0)
  }
  isSorted(Array(7, 9, 13), (x: Int, y: Int) => x < y)
}