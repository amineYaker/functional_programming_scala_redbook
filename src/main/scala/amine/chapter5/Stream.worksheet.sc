import fansi.Str
import scala.annotation.tailrec

trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h()) // forcing the "h" Thunk explicitly
  }

  // tail rec impl (the trick is to reverse)
  def toList: List[A] = {

    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse

  }

  /*def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => Cons(h, t().take(n - 1))
    case Cons(h, _) if n == 1 => Cons(h, empty)
    case _                    => Empty
  }*/
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (f(h())) => Cons(h, () => t() takeWhile f)
    case _                      => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def existsWithRecursion(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().existsWithRecursion(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty      => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) Stream.cons(h, t) else Stream.empty
    )

  def headOptionViaFR: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)

  // Infinite streams

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      Stream.cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => Stream.empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  def fibsViaUnfold(n: Int): Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some((a, a)))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some((h(), (Stream.empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), ((t1(), t2()))))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(
      f: (Option[A], Option[B]) => C
  ): Stream[C] = unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) =>
      Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
    case (Empty, Cons(h, t)) =>
      Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll { case (h, h2) =>
      h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s     => Some((s, s drop 1))
  } append (Stream(Stream.empty[A]))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // HARD: generelize tails to scanRight which is like a foldRigh that returns intermediate results with O(n) complexity
  // we cannot use unfold because unfold computes result from left to right
  // we can use foldRIGHT and keep the stream of intermediate results in the state

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {

      // p0 is passed by name and used in by name args in f and cons
      // we need lazy val to memoize
      lazy val p1 = p0

      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd // memoizing
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
