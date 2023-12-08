package amine.chapter11

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

// basic blocs for Monad are flatMap and Unit and it's useful to redefine map & map2

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // 11.3 sequence and traverse

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)
  // lma match {
  //   case head :: next => flatMap(head)((hh => map(sequence(next))(hh :: _)))
  //   case Nil          => unit(Nil)
  // }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  // ALTERNATIVE
  // la match {
  //   case head :: next => map2(f(head), traverse(next)(f))(_ :: _)
  //   case Nil          => unit(Nil)
  // }

  // 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // recursive not tail recursive
  // if (n <= 0) unit(List[A]())
  // else map2(ma, replicateM(n - 1, ma))(_ :: _)

  // 11.5

  // replicatedM for lists results in all possible combinations of n elements ( pow(2,n) lists inside a list)
  // replicatedM for options results in :
  // None if we replicated None with n > 0
  // Some( List(x , x repeated n times)) if we replicate a Some(x) n times
  // Some(List()) if we replicate None 0 times

  // replicate M repeats the supplied monadic value n times
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    // Works first try
    // ms.foldLeft(unit(List[A]()): F[List[A]])((acc, a) =>
    //   map2(flatMap(f(a))(b => if (b) unit(List(a)) else unit(List[A]())), acc)(
    //     _ ++ _
    //   )

    // )

    // clean version from answers
    ms.foldRight(unit(List[A]()): F[List[A]])((a, acc) =>
      flatMap(f(a))(b => if (b) map2(unit(a), acc)(_ :: _) else acc)
    )

  // Kleisli composition function
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  // 11.8 flatMap via compose

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose[Unit, A, B](_ => ma, f)(())

  // proving monad laws .. seen

  // 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  // 11.13
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(a => f(a)))

  // the identity monad

}
