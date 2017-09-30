package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A,B) => C) extends Future[C] {

    private var cache: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def isDone: Boolean = cache.isDefined

    override def get(timeout: Long, unit: TimeUnit): C = {
      evaluate(timeout, unit)
    }

    private def evaluate(timeout: Long, unit: TimeUnit = TimeUnit.NANOSECONDS): C = {
      cache match {
        case Some(r) => r
        case None =>
          val timeOutInNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
          val start = System.nanoTime()
          val ar = a.get(timeOutInNanos, unit)
          val aStop = System.nanoTime() - start
          val br = b.get(timeOutInNanos - aStop, unit)
          val result = f(ar, br)
          cache = Some(result)
          result
      }
    }

    override def get: C = evaluate(Long.MaxValue)
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft[Par[List[A]]](unit(List[A]())) ( (acc, par) =>
      map2(acc, par)(_ :+ _)
    )
  }

  private def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[List[A]] = fork {
    if (ps.isEmpty) unit(List[A]())
    else if (ps.length == 1) map(ps.head)(a => List(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence2[A](as: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence2(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par.Par[List[A]]] = as.map(asyncF((l: A) => if(f(l)) List[A](l) else List()))
    map(sequence2(pars))(_.flatten)
  }

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val i = run(es)(n).get()
      run(es)(choices(i))
    }
  }

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(choices)
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))
  }

  def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(map(cond)(b => if (b) 0 else 1))(List(t, f))
  }

  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(f(k))
    }

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(f(k))
    }

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def join[A](p: Par[Par[A]]): Par[A] = {
    es =>
      val par: Par[A] = run(es)(p).get()
      run(es)(par)
  }

  def joinViaFlatMap[A](p: Par[Par[A]]): Par[A] =
    flatMap(p)(a => a)


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
