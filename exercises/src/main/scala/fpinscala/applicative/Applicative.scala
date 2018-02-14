package fpinscala
package applicative

import fpinscala.applicative.StateUtil._
import fpinscala.monads.{Functor, Id}
import fpinscala.monoids._
import fpinscala.state._

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])
                   (f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def product[G[_]](G: Applicative[G])
  : Applicative[({type f[x] = (F[x], G[x])})#f] =

    new Applicative[({type f[x] = (F[x], G[x])})#f] {

    override def unit[A](a: => A): (F[A], G[A]) = {
      (self.unit(a), G.unit(a))
    }

      override def map2[A, B, C]
      (fa: (F[A], G[A]), fb: (F[B], G[B]))
      (f: (A, B) => C)
      : (F[C], G[C]) = {

        val fc: F[C] = self.map2(fa._1, fb._1)(f)
        val gc: G[C] = G.map2(fa._2, fb._2)(f)

        (fc, gc)
      }
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fa, fb)(G.map2(_, _)(f))
  }

  // 10.01.2018
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map.empty[K,V])){
      case(facc, (k, fv)) =>
        map2(facc, fv)((acc, v) => acc + (k -> v))
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {

    override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B]
      (ma: Either[E, A])
      (f: A => Either[E, B])
      : Either[E, B] = {
        ma.flatMap(f)
      }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] =
      State(s => (a, s))

    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]]
  (implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = new Monad[({type f[x] = F[N[x]]})#f] {

    override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

    override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] = {
      F.flatMap(ma){ (x: N[A]) =>
        F.map(T.traverse(x)(a => f(a)))(N.join)
      }
    }
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

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]
  : Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A,B,C](fa: Validation[E, A ], fb: Validation[E, B])(f: (A,B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => unit(f(a,b))
      case (Failure(ae, aes), Failure(be, bes)) => Failure(ae, (aes :+ be) ++ bes)
      case (a@Failure(_,_), _) => a
      case (_, b@Failure(_,_)) => b

     }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])
                                    (implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fma: F[G[A]])
                                  (implicit G: Applicative[G]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    implicit val idApp = new Applicative[Id] {
      override def unit[A](a: => A): Id[A] =
        a

      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
        f(fa, fb)
    }

    traverse(fa)(x => idApp.unit(f(x)))
  }


  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  }

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(fa, z)((a: A, acc: B) => ((), f(acc, a)))._2
  }

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {

    val GHprod = G.product(H)
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(GHprod)
  }

  def compose[G[_]](implicit G: Traverse[G])
  : Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[AG[_], A, B](fa: F[G[A]])(f: A => AG[B])(implicit AG: Applicative[AG]): AG[F[G[B]]] = {
        self.traverse(fa)(x => G.traverse(x)(f))
      }
    }
  }
}

object Traverse extends App {

  val listTraverse = new Traverse[List]() {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldLeft(G.unit(List.empty[B])){(facc: G[List[B]], a: A) =>
        G.map2(facc,f(a))((a: List[B], b: B) => a :+ b)
      }
  }

  val res = listTraverse.foldLeft(List(1,2,3))(0)(_ + _)
//  println("res = " + res)

  implicit  val appOption = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C]
    (fa: Option[A], fb: Option[B])
    (f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb.map(b => f(a,b)))
  }

  val res2 = listTraverse.fuse[Option,Option, Int, Int](
    List(1,2,3)
  )(
    a => Option(a),
    b => if ((b % 2) == 0) Some(b) else None
  )

//  println("res2 = " + res2)

  val optionTraverse = new Traverse[Option]() {
    override def traverse[G[_], A, B]
    (fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa.foldLeft(G.unit(Option.empty[B]))((facc, a) =>
        G.map2(facc, f(a))((a: Option[B], b: B) => a.orElse(Option(b)))
      )
  }

  val res3 = listTraverse.compose(optionTraverse)
    .traverse(List(Some(1), None, Some(2)))(i => Option(i).filter(_ % 2 == 0))

  val res4 = optionTraverse.compose(listTraverse)
    .traverse(Some(List(1,2,3)))(i => Option(i))

  println(s"res3 = $res3")
  println(s"res4 = $res4")

  val treeTraverse = new Traverse[Tree]() {
    override def traverse[G[_], A, B]
    (fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(
        f(fa.head),
        listTraverse.traverse(fa.tail)(tree => traverse(tree)(f)))((b, list) => Tree(b, list)
      )
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
