import amine.chapter7
trait Monoid[A] {
  def op(a1: A, a2: A): A // Satisfies op(op(x,y), z) == op(x, op(y,z))
  def zero: A // Satisfies op(x,zero) == x and op(zero,x) == x
}

val stringMonoid = new Monoid[String] {

  def op(a1: String, a2: String): String = a1 + a2
  def zero: String = ""
}

val intAddition = new Monoid[Int] {
  def op(a1: Int, a2: Int): Int = a1 + a2
  def zero: Int = 0
}

val intMultiplication = new Monoid[Int] {
  def op(a1: Int, a2: Int): Int = a1 * a2
  def zero: Int = 1
}

val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  def zero: Boolean = false
}

val booleanAnd = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  def zero: Boolean = true
}

def optionMonoid[A](implicit monoidA: Monoid[A]): Monoid[Option[A]] =
  new Monoid[Option[A]] {

    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2 // solution

    // should be valid too
    // def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
    //   case (None, None)         => None
    //   case (Some(a1), None)     => Some(a1)
    //   case (None, Some(a2))     => Some(a2)
    //   case (Some(a1), Some(a2)) => Some(monoidA.op(a1, a2))
    // }

    def zero: Option[A] = None
  }

def endoMonoid[A]: Monoid[A => A] =
  new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A =
      a1 andThen (a2) // or a1 compose a2 (the DUAL monoid)
    def zero: A => A = x => x
  }

def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(a1: A, a2: A): A = m.op(a2, a1)
  def zero: A = m.zero
}

// we can create a concatenate that combines monoid ops with folding in a list

def concatenate[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  concatenate(as.map(f), m)

//foldMap via foldLeft

def foldMapViaFoldLeft[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.map(f).foldLeft(m.zero)(m.op)

def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(op: (B, A) => B)(implicit
    endoMonoid: Monoid[B => B]
): B =
  foldMap(as, endoMonoid)(a => b => op(b, a))(z)

//10.7 balanced fold

def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if (v.isEmpty) {
    m.zero
  } else {
    if (v.length == 1) { f(v(0)) }
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  }
}

import amine.chapter7.Par
import amine.chapter7.Par._

def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
  def zero: Par[A] = Par.unit(m.zero)
}

def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???
/*

  //from the solutions
  Par.parMap(as)(f).flatMap(bs =>
    foldMapV(bs, par(m)) (b => Par.lazyUnit(b)))

 */

//10.9 foldMap to detect whether a given IndexedSeq[Int] is ordered

// treat it as a map reduce problem : split the sequence in half check that each half is ordered and when we combine we check that
// the max of left half is less or equal than the min of right half

case class Interval(ordered: Boolean, min: Int, max: Int)

val orderedMonoid: Monoid[Option[Interval]] = new Monoid[Option[Interval]] {
  def op(oa1: Option[Interval], oa2: Option[Interval]): Option[Interval] =
    oa1 -> oa2 match {
      case (Some(a1), Some(a2)) =>
        Some(
          Interval(a1.ordered && a2.ordered && a1.max <= a2.max, a1.min, a2.max)
        )
      case (x, None) => x
      case (None, y) => y
    }

  def zero: Option[Interval] = None
}

def ordered(ints: IndexedSeq[Int]): Boolean =
  foldMapV(ints, orderedMonoid)(i => Some(Interval(true, i, i)))
    .map(_.ordered)
    .getOrElse(true)

val strFile = "lorem ipsum dolor sit amet, "

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  override def op(a1: WC, a2: WC): WC = (a1, a2) match {
    case (Stub(c1), Stub(c2))      => Stub(c1 + c2)
    case (Stub(c1), Part(l, w, r)) => Part(c1 + l, w, r)
    case (Part(l, w, r), Stub(c2)) => Part(l, w, r + c2)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)

  }

  override def zero: WC = Stub("")
}

def countWords(data: String)(implicit wcMonoid: Monoid[WC]): Int = {
  def wc(c: Char): WC =
    if (c.isWhitespace)
      Part("", 0, "")
    else
      Stub(c.toString)

  def unstub(s: String) = s.length min 1

  foldMapV(data.toIndexedSeq, wcMonoid)(wc) match {
    case Stub(chars)               => unstub(chars)
    case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
  }

}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      case Leaf(value)         => f(z, value)
    }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Branch(left, right) =>
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      case Leaf(value) => f(value)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      case Leaf(value)         => f(value, z)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None    => z
      case Some(a) => f(z, a)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None    => mb.zero
      case Some(a) => f(a)
    }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None    => z
      case Some(a) => f(a, z)
    }
}

def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
  new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
  override def zero: A => B = _ => B.zero
  override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
}

def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
      }
  }
def bag[A](as: IndexedSeq[A]): Map[A, Int] =
  foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
