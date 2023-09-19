trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
// case class Left[+E](value: E) extends Either[E, Nothing]
// case class Right[+A](value: A) extends Either[Nothing, A]

// object Either extends Either[Int, String] {
//   override def map[B](f: String => B): Either[Int, B] =
//     this match {
//       case Left(value)  => Left(value)
//       case Right(value) => Right(f(value))
//     }

//   override def flatMap[EE >: E, B](f: String => Either[EE, B]): Either[EE, B] =
//     this match {
//       case Left(value)  => Left(value)
//       case Right(value) => f(value)
//     }

//   override def map2[EE >: E, B, C](
//       b: Either[EE, B]
//   )(f: (A, B) => C): Either[EE, C] =
//     for {
//       a <- this
//       b1 <- b
//     } yield f(a, b1)
//   override def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
//     this match {
//       case Left(_)      => b
//       case Right(value) => Right(value)
//     }

//   def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
//     es match {
//       case head :: next => (f(h) map2 traverse(t)(f))(_ :: _)
//       case Nil          => Right(Nil)
//     }

// }
