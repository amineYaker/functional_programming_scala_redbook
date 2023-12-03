import amine.chapter6.State
trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // 12.1

  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  def replicatedM[A](n: Int)(fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => a -> b)

  // 12.2 apply via unit & map2

  def apply[A, B](fab: F[A => B], fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f), fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried), fa), fb)

  // 12.3 map3 and map4 we can always define mapN in terms of mapN-1, we can also use the same pattern by chaining applys ...

  // 12.8

  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {

    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {

      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](
          fab: (F[A => B], G[A => B]),
          fa: (F[A], G[A])
      ): (F[B], G[B]) =
        self.apply(fab._1, fa._1) -> G.apply(fab._2, fa._2)

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
          f: (A, B) => C
      ): (F[C], G[C]) =
        apply(apply(unit(f.curried), fa), fb)
    }
  }
  def compose[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = {

    val self = this

    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
          f: (A, B) => C
      ): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }

}

type Id[A] = A

val idMonad = new Monad[Id] {
  def unit[A](a: => A): Id[A] = a
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
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

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)

  // foldable cannot extend functor because it cannot construct an F of Foldable type via foldRight foldLeft and foldMap
}

// turning a Monoid into an applicative
trait Monoid[A] {
  def op(a1: A, a2: A): A // Satisfies op(op(x,y), z) == op(x, op(y,z))
  def zero: A // Satisfies op(x,zero) == x and op(zero,x) == x
}

type Const[M, B] = M

implicit def monoidApplicative[M](M: Monoid[M]) =
  new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero
    def map2[A, B, C](fa: M, fb: M)(f: (A, B) => C): M = M.op(fa, fb)
  }

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  // we need the id Monad
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)
  def traverse[G[_]: Applicative, A, B](ta: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(ta)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      def map2[A, B, C](fa: M, fb: M)(f: (A, B) => C): M = M.op(fa, fb)
    }
  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      (for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b)
    ).run(s)

  override def toList[A](as: F[A]): List[A] =
    mapAccum(as, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // 12.16

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(
      f: A => G[B],
      g: A => H[B]
  )(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(
      G product H
    )

  // composition of Traverse
  def compose[G[_]](implicit
      G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] = {
    val self = this

    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[M[_]: Applicative, A, B](ta: F[G[A]])(
          f: A => M[B]
      ) =
        self.traverse(ta)((ga: G[A]) => G.traverse(ga)(f))
    }
  }
  // MONAD COMPOSITION

  def compsoeM[G[_], H[_]](implicit
      G: Monad[G],
      H: Monad[H],
      T: Traverse[H]
  ): Monad[({ type f[x] = G[H[x]] })#f] =
    new Monad[({ type f[x] = G[H[x]] })#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mfa: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mfa)(fa => G.map(T.traverse(fa)(f))(H.join))
    }
}

def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
  override def unit[A](a: => A): State[S, A] = State(s => (a, s))

  override def flatMap[A, B](ma: State[S, A])(
      f: A => State[S, B]
  ): State[S, B] = ma.flatMap(f)

}
trait Monad[F[_]] extends Applicative[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def unit[A](a: => A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

}

// 12.4

//Applicative of Streams turns a List of possibly infinite streams to a possibly infinite stream of lists containing the elements zipped
// it transposes the list

//12.5

def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
  new Monad[({ type f[x] = Either[E, x] })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](
        fa: Either[E, A]
    )(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }
  }

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

def validationApplicative[E]
    : Applicative[({ type f[x] = Validation[E, x] })#f] =
  new Applicative[({ type f[x] = Validation[E, x] })#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
        f: (A, B) => C
    ): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e @ Failure(_, _), _) => e

        case (_, e @ Failure(_, _)) => e
      }
  }

// val listTraverse = new Traverse[List] {
//   override def traverse[G[_]: Applicative, A, B](ta: List[A])(
//       f: A => G[B]
//   ): G[List[B]] =
//     ta.foldLeft(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
// }

// val optionTraverse = new Traverse[Option] {
//   override def traverse[G[_]: Applicative, A, B](
//       ta: Option[A]
//   )(f: A => G[B]): G[Option[B]] = {
//     ta match {
//       case Some(a) => G.map(f(a))(Some(_))
//       case None    => G.unit(None)
//     }
//   }
// }
