def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

  def loop(idx: Int, prev: A): Boolean =
    if (idx >= as.length) true
    else {
      if (ordered(prev, as(idx))) loop(idx + 1, as(idx))
      else false

    }
  if (as.size == 0) true
  else
    loop(1, as(0))
}

val arr: Array[Int] = Array(1, 2, 3)
val sortFun: (Int, Int) => Boolean = (x, y) => if (x < y) true else false
isSorted(arr, sortFun)
val arrEmpt: Array[Int] = Array()
isSorted(arrEmpt, sortFun)
