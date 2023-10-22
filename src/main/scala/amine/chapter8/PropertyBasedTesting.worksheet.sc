import java.util.concurrent.Executors
import amine.chapter7.Par
import amine.chapter6._

// here chapter 8 starts
case class Gen[A](sample: State[RNG, A]) {

  import amine.chapter6.Rand._

//8.4

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State[RNG, Boolean](rng => rng.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequenceViaFoldLeft(List.fill(n)(g.sample)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    flatMap(a => unit(f(a)))

  def map2[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] =
    ga.flatMap(a => gb.map(b => f(a, b)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State[RNG, Double](rng => rng.double).flatMap(d =>
        if (d < threshold) g1._1.sample else g2._1.sample
      )
    )
  }

  def unsized: SGen[A] = SGen(_ => this)

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1, g))

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def **[B](g: Gen[B]): Gen[(A, B)] = (map2(this, g))((_, _))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State[RNG, Int](rng => nonNegativeInt(rng)).map(n =>
        start + n % (stopExclusive - start)
      )
    )
}

//PROP
type TestCases = Int
type FailedCase = String
type SuccessCount = Int
type MaxSize = Int

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  def isFalsified: Boolean = true
}

case object Proved extends Result {
  def isFalsified: Boolean = false
}

case class SGen[A](g: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] =
    SGen(g andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))

  def listOfN[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n, g))

}
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.map(x => x))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props
        .map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)

      prop.run(max, n, rng)
  }

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack tracte: \n ${e.getStackTrace.mkString("\n")}"

  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x               => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(failure, _) => p.tag(failure).run(max, n, rng)
      case Passed                => Passed
      case Proved                => Proved
    }
  }

  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(failure, successes) =>
        Falsified(msg + "\n" + failure, successes)
      case x => x
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

//8.14

  /*

sorted list is either empty has one element or for evert two consecutive elements (a,b) we have a > b

// sorted list has all elements of input list
// sorted list doesn't have element that doesn't exist in the input list */
  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) =>
        println(s"! Falsified after $successes passed tests: \n $failure")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property")
    }

  val ES = Executors.newFixedThreadPool(4)
  val p3 = check {
    Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  import amine.chapter7.Par

  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
    forAll(g.S.**(g)) { case (s, a) => f(a)(s).get }

  // 8.16 (copied from blue book)

  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   * computation for each element of the input list summed to produce the final
   * result. This is not the most compelling example, but it provides at least
   * some variation in structure to use for testing
   */

  val pint2: Gen[Par.Par[Int]] = ???
  /*
  choose(-100,100).listOfN(choose(0,20)).map(l => l.foldLeft(Par.unit(0)) (p, i) =>
  Par.fork{Par.map2(p, Par.unit(i))(_ + _ )}
  )
   */

  val forkProp = forAllPar(pint2)(i => Par.equal(Par.fork(i), i)) tag "fork"

}
