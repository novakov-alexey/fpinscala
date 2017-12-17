package fpinscala
package applicative

import fpinscala.applicative.StateUtil._
import fpinscala.monads.Functor
import fpinscala.monoids._
import fpinscala.state._

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val value: F[B => C] = map(fa)(f.curried)
    apply(value)(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    val value: F[B => C => D] = map(fa)(f.curried)
    val value1: F[C => D] = apply(value)(fb)
    val value2 = apply(value1)(fc)
    value2
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
  }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, b) => ab(b))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A])) {
      (ma, acc) => map2(ma, acc)(_ :: _)
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))
                              (fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {

      override def unit[A](a: => A): (F[G[A]]) = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)((a, b) => G.map2(a, b)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) {
      (acc, e) => map2(acc, e._2)((ac, v: V) => ac + (e._1 -> v))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))


  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      //override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
      //self.flatMap(fga)(ga => G.flatMap(ga)(a => f(a))) - not possible!
    }
  }

}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      eea match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] =
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def flatMap[A, B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
        F.flatMap(fna)(na => F.map(T.traverse(na)(f))(N.join))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A],
                               fb: Validation[E, B])
                              (f: (A, B) => C): Validation[E, C] = {
      fa match {
        case Success(a) => fb match {
          case Success(b) => Success(f(a, b))
          case f@Failure(h, t) => f
        }
        case f@Failure(h, t) => fb match {
          case Success(_) => f
          case Failure(h2, t2) => Failure(h, t ++ Vector(h) ++ t2)
        }
      }
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  type Id[A] = A // trick!

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): A = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def traverse[G[_], A, B](fa: F[A])
                          (f: A => G[B])
                          (implicit G: Applicative[G]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  def sequence[G[_], A](fma: F[G[A]])
                       (implicit G: Applicative[G]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a: A, b: B) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])
                                     (f: A => G[B])
                                     (implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B])) {
        (a, acc) => G.map2(f(a), acc)(_ :: _)
      }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])
                                     (f: A => G[B])
                                     (implicit G: Applicative[G]): G[Option[B]] = {
      oa.fold(G.unit(Option.empty[B]))(a => G.map(f(a))(b => Some(b)))
    }

  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B]
    (fta: Tree[A])
    (f: A => G[B])
    (implicit G: Applicative[G]): G[Tree[B]] = {
      G.map2(f(fta.head), listTraverse.traverse(fta.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
