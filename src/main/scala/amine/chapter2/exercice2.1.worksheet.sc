def fib(n: Int): Int = {
  def go(n: Int, acc: Int): Int =
    if (n < 1) acc
    else {
      if (n == 1) 1
      else {
        go(n - 1, acc) + go(n - 2, acc)
      }
    }
  go(n, 0)
}

//extra: tail recursive version
def fibTailRec(n: Int): Int = {
  @scala.annotation.tailrec
  def go(n: Int, acc1: Int, acc2: Int): Int = n match {
    case 0 => acc1
    case 1 => acc2
    case _ => go(n - 1, acc2, acc1 + acc2)
  }
  go(n, 0, 1)

}
