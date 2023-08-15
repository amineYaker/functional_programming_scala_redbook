sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    def size[A](t: Tree[A]): Int = 
        t match
            case Leaf(_) => 1 
            case Branch(left, right) => 1 + size(left) + size(right)

    def maximum(t: Tree[Int]): Int = t match
        case Leaf(value) => value
        case Branch(left, right) => maximum(left) max maximum(right)

    def depth[A](t: Tree[A]) : Int = t match
        case Leaf(value) => 0
        case Branch(left, right) => 1 + (depth(left) max depth(right))
    
    def map[A,B](t: Tree[A])(f : A => B) : Tree[B] = t match
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    
    def fold[A,B](t: Tree[A])(f: A =>B)(g: (B,B) => B) : B = t match
        case Leaf(value) => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g) )

    /*
    * The analogy between this fold and the foldRight for lists is that: fold receives a "handler" for each data constructors of the type
    * and recursively accumulates some value using these handlers. As with foldRight, fold(t)(Leaf(_-))(Branch(_,_)) == t, and we can 
    * use this function to implement any recursive function that would otherwise be defined by pattern matching. 
    */

    def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)(a => Leaf(f(a)): Tree[B] )(Branch(_, _))

    def sizeViaFold[A](t: Tree[A]): Int = 
        fold(t)(a => 1)( 1 + _ + _)
        
}


val testTree: Tree[Int] = Branch[Int]( Leaf[Int](2), Leaf[Int](3)) 

val max = Tree.maximum(testTree)