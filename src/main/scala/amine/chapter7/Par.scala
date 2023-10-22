package amine.chapter7

import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
// Exercice 7.01

/* def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C) : Par[C]*/

object Par {

// Basic implementation of Par
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone(): Boolean = true
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled(): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // 7.3 complex and low level ...

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => unit(f(a))

  // 7.5 HARD
  // we can have a balanced implementation if we have IndexedSeq
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF((a: A) => if (f(a)) List(a) else List()))

    map(sequence(pars))(_.flatten)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // doesn't work if thread pool size is 1
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // Actor: non-blocking concurrency primitive: a concurrent process that doesn't constantly occupy a thread
  // only occupy a thread when it receives a message and it parses one message at a time

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val idx = run(es)(n).get
      choices(idx)(es)
    }

  // choiceMap
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k: K = run(es)(key).get
      run(es)(choices(k))
    }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val choice = run(es)(pa).get()
      run(es)(choices(choice))
    }

  // chooser is called flatMap or Bind in FP

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val choice = run(es)(pa).get()
      run(es)(choices(choice))
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap[Boolean, A](cond)(b => if (b) t else f)

  def join[A](pa: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(pa).get())

  def joinViaFlatMap[A](pa: Par[Par[A]]): Par[A] =
    flatMap(pa)(x => x)

  def flatMapViaJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def equals[A](p1: Par[A], p2: Par[A]): Boolean = ???

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

}
