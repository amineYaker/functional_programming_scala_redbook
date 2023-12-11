sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

//Algebra of mutable references
// STRef

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      protected def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    protected def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) { case ((k, v), st) =>
      st flatMap (_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

  def noop[S] = ST[S, Unit](())

  def partition[S](
      arr: STArray[S, Int],
      n: Int,
      r: Int,
      pivot: Int
  ): ST[S, Int] =
    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- STRef(n)

      _ <- (n until r).foldLeft(noop[S])((s, i) =>
        for {
          _ <- s
          vi <- arr.read(i)
          _ <-
            if (vi < pivotVal) (for {
              vj <- j.read
              _ <- arr.swap(i, vj)
              _ <- j.write(vj + 1)
            } yield ())
            else noop[S]
        } yield ()
      )
      x <- j.read
      _ <- arr.swap(x, r)
    } yield x

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield ()
  else noop[S]

  def quickSort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  // 14.1
  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })

}
