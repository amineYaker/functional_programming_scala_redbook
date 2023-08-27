def sequence[Int](a: List[Option[Int]]): Option[List[Int]] = 
     a match
        case head :: next =>  head flatMap (hh => sequence(next) map (hh :: _))
        case Nil => Some(Nil)
     
    
    

val test = sequence(List(Some(2), Some(3)))

val test2 = sequence(List(None, Some(3)))


def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))


def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match
        case head :: next => map2(f(head), traverse(next)(f))(_ :: _ ) 
        case Nil => Some(Nil)
     
def sequenceViaTraverse[A](a: List[Option[A]]) : Option[List[A]] = 
    traverse(a)(x => x)