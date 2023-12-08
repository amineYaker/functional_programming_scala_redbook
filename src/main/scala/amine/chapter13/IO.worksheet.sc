import amine.chapter7.Par.Par
import amine.chapter7.{Par => Parallelism}
import amine.chapter11.Monad

sealed trait Free[F[_], A]

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
    extends Free[F, B]

type TailRec[A] = Free[Function0, A]

type Async[A] = Free[Par, A]

object Free {

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] = {
    val self = this
    new Monad[({ type f[a] = Free[F, a] })#f] {
      override def flatMap[A, B](ma: Free[F, A])(
          f: A => Free[F, B]
      ): Free[F, B] = FlatMap(ma, f)
      override def unit[A](a: => A): Free[F, A] = Return[F, A](a)

      override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] =
        flatMap(fa)(a => Return(f(a)))
    }
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)  => a
    case Suspend(s) => s()
    case FlatMap(s, f) =>
      f() match {
        case FlatMap(s, f) =>
          runTrampoline(
            freeMonad.flatMap(s)(y => freeMonad.flatMap(f(y))(f))
          )
        case Suspend(s) => runTrampoline(f(s()))
        case Return(a)  => runTrampoline(f(a))
      }
  }

  @annotation.tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) =>
      step(freeMonad.flatMap(x)(a => freeMonad.flatMap(f(a))(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _                     => free
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] =
    step(a) match {
      case Return(a)  => F.unit(a)
      case Suspend(s) => s
      case FlatMap(s, f) =>
        s match {
          case Suspend(s) => F.flatMap(s)(a => run(f(a)))
        }
    }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar: Par[Option[String]] = Parallelism.unit(run)
    def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some("readLine")
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar: Par[Unit] = Parallelism.unit(line)
    def toThunk: () => Unit = () => println(line)
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](f: Console[A]): () => A = f.toThunk
  }

  def runFree[F[_], G[_], A](
      free: Free[F, A]
  )(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(s)             => t(s)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) = () =>
      f(a())()
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G)(implicit
      G: Monad[G]
  ): Free[G, A] =
    Suspend(runFree(f)(fg))

  def runConsole[A](fa: Free[Console, A]): A = runTrampoline(
    translate(fa)(consoleToFunction0)
  )
}
