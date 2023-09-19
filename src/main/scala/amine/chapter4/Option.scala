trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): Option[B]
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]

}
// case class Some[+A](get: A) extends Option[A]

// case object None extends Option[Nothing]

// object Option {
//   def map[A, B](f: A => B): Option[B] = this match {
//     case Option  => None
//     case Some(a) => Some(f(a))
//   }

//   def getOrElse[B >: A](default: => B): B = this match {
//     case None    => default
//     case Some(a) => a
//   }

//   def flatMap[B](f: A => Option[B]): Option[B] =
//     map(f) getOrElse None

//   def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
//     case None => ob
//     case _    => this
//   }

//   def filter(f: A => Boolean): Option[A] = this match {
//     case Some(a) if f(a) => this
//     case _               => None
//   }

//   def mean(xs: Seq[Double]): Option[Double] =
//     if (xs.isEmpty) None
//     else Some(xs.sum / xs.length)

//   def variance(xs: Seq[Double]): Option[Double] =
//     mean(xs).flatMap(m => mean(xs.map(x => math.pow(w - m, 2))))

//   def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

//   def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//     a flatMap (aa => b map (bb => f(aa, bb)))
// }
