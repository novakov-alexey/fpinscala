package fpinscala
package monads

import fpinscala.parallelism.Par._
import fpinscala.parallelism._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma.foldRight(unit(List.empty[A])){
    (ma,acc) =>
      map2(ma, acc)(_ :: _)
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = sequence(la.map(f(_)))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill(n){ma})
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_:Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(a => a)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = ???

    override def unit[A](a: => A): P[A] = ???
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)

    override def unit[A](a: => A): Option[A] = Option(a)
  }

  val streamMonad: Monad[Stream] = ???

  val listMonad: Monad[List] = ???
// monad(f: A => B
// state[A](s: Int, a: A)
// monad({ val lambda = x => state(42, x)}.lambda)
  def stateMonad[S]
  : Monad[({type Lambda[A] = State[S, A]})#Lambda] =
    new Monad[({type Lambda[A] = State[S, A]})#Lambda] {

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)

    override def unit[A](a: => A): State[S, A] = State.unit(a)
  }


  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)

    override def unit[A](a: => A): Id[A] = Id(a)
  }

  val F = stateMonad[Int]
  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  def zipWithIndex[A](as: List[A]): List[(Int,A)] = {
    val tuple: (List[(Int, A)], Int) = as.foldLeft(F.unit(List[(Int, A)]()))((acc: State[Int, List[(Int, A)]], a: A) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)
    tuple._1.reverse
  }

  def readerMonad[R] = Reader.readerMonad
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => {
        val a: A = st.run(r)
        f(a).run(r)
      })
  }
}

object Main extends App {

  val r = Reader[List[Int], Int](l => l.length)
  println(r.run(List(3,3,3,3)))
}
