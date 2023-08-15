
sealed trait List[+A] 
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match
        case Nil => 0
        case Cons(head, tail) => head + sum(tail)
    
    def product (ds: List[Double]): Double = ds match
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(head, tail) => head * product(tail)
    
    def apply[A](as: A*): List[A] = 
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    //3.1 => answer is 3    
    //3.2
    def tail[A](l: List[A]): List[A] = 
        l match
            case Nil => Nil
            case Cons(head, tail) => tail
    
    //3.3
    def setHead[A] (l : List[A], newHead: A): List[A] =
        l match
            case Nil => Cons(newHead, Nil)
            case Cons(head, tail) => Cons(newHead, tail)
        
    //3.4
    def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l 
        else {
            drop(tail(l), n - 1)
        }

    //3.5    
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
        l match
            case Nil => Nil 
            case Cons(head, tail) => if f(head) then dropWhile(tail, f) else Cons(head, tail )
         
   def append[A](a1: List[A], a2: List[A]): List[A] = 
        a1 match
            case Nil => a2
            case Cons(head, tail) => Cons(head, append(tail, a2))

    def init[A](l : List[A]): List[A] = 
        l match
            case Nil => Nil 
            case Cons(head, Nil) => Nil  
            case Cons(head, tail) => Cons(head, init(tail))

    def foldRight[A, B](as: List[A], z: B)(f: (A,B) => B): B = 
        as match
            case Nil => z
            case Cons(head, tail) => f(head, foldRight(tail,z)(f))
        
    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) = 
        foldRight(ns , 1.0)( _ * _)
            
        
    // 3.7 we cannot stop as the in order for the function of a foldright to be evaluated we need to evaluate its arguments by traversing the list before outputting a result 
    // we need a non strict evaluation
    
    //3.8 we get the original List

    //3.9
    def length[A](as: List[A]): Int = 
        foldRight(as, 0)( (_, acc) => acc + 1)  

    def foldLeft[A,B](as: List[A], z: B) (f: (B,A) => B): B =
        as match
            case Nil => z
            case Cons(head, tail) => foldLeft(tail,f(z, head))(f)
        
    //3.11 sum, product, length using foldLeft
    def sumFL(as: List[Int]): Int = 
        foldLeft(as,0)((acc, x) => acc + x) 
    
    def productFL(as: List[Double]): Double = 
        foldLeft(as, 1.0)((acc,x) => acc * x)

    def lengthFL[A](as: List[A]): Int =
        foldLeft(as,0)((acc, _) => acc + 1)

    //3.12 reverse
    def reverse[A](as: List[A]): List[A] = 
        foldLeft(as, List[A]())((acc, c) => Cons(c, acc))
    
    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(reverse(as), z)((b,a) => f(a,b))
    
    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_,_))

    def concat[A](l : List[List[A]]) : List[A] = 
        foldRight(l, Nil : List[A])(append)

    def add1(l : List[Int]): List[Int]=
        foldRight(l, Nil: List[Int])((h,t) => Cons(h+1, t))
    
    def doubleToString(l: List[Double]): List[String] = 
        foldRight(l, Nil: List[String])((h,t) => Cons(h.toString, t))

    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRightViaFoldLeft(as, Nil: List[B])((h,t)=> Cons(f(h), t))
    
    def filter[A](as: List[A])(f: A => Boolean): List[A] = 
        foldRightViaFoldLeft(as, Nil: List[A])((h, t) => if (f(h)) Cons(h,t ) else t)

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      concat(map(as)(f))

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)( a => if (f(a)) List(a) else Nil)

    def zipWith[A,B,C](a1: List[A], a2: List[B])(f : (A, B) => C) : List[C] = 
        (a1, a2) match
            case (Nil, _) => Nil 
            case (_, Nil) => Nil 
            case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
            
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match
            case (_, Nil) => true
            case (Nil , Cons(_,_ )) => false 
            case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) startsWith(t1,t2) || hasSubsequence(t1, Cons(h2, t2))  else hasSubsequence(t1,Cons(h2,t2)) 

    // helper function to shortcut the hasSubsequence function        
    def startsWith[A](sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match
            case (_, Nil) => true 
            case (Nil, Cons(_,_)) => false
            case (Cons(h1,t1), Cons(h2, t2)) => if (h1 != h2) false else startsWith(t1, t2)
        
         
}  

val test = List(1,2,3)
val test2 = List(4,5,6)

val t1 = List(1,2,3)
val subT1 = List(2,3)

List.hasSubsequence(t1, subT1)

val stWith = List.startsWith(test, t1)
val testZipWith = List.zipWith(test,test2)((a,b) => a +b )

val s = List.sumFL(test)
val l = List.lengthFL(test)
val res = List.dropWhile(test, x => x <2)

val res2 = List.init(test)
val res3 = List.reverse(test) 

val testMap = List.filter(test)(x => x%2 == 0)