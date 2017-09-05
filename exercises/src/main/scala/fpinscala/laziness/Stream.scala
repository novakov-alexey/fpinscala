package fpinscala.laziness

import fpinscala.laziness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight()
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList2: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Cons(h, t) => loop(t(), acc :+ h())
        case _ => acc
      }
    }

    loop(this, List())
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    def loop(n: Int, acc: Stream[A]): Stream[A] = {
      acc match {
        case Cons(h, t) if n > 1 => cons(h(), loop(n - 1, t()))
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _ => empty
      }
    }

    loop(n, this)
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((n, this)) {
      case (count, Cons(t, h)) if count > 0 => Some(t(), (count - 1, h()))
      case _ => None
    }
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(n: Int, acc: Stream[A]): Stream[A] = {
      acc match {
        case Cons(h, t) if n > 0 => loop(n - 1, t())
        case _ if n <= 0 => acc
      }
    }

    loop(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(p: A => Boolean, acc: Stream[A]): Stream[A] = {
      acc match {
        case Cons(h, t) if p(h()) => cons(h(), loop(p, t()))
        case _ => empty
      }
    }

    loop(p, this)
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(t, h) if p(t()) => Some(t(), h())
      case _ => None
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => {
      if (p(a)) cons(a, b)
      else empty
    })
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(t, h), Cons(t2, h2)) => Some(f(t(), t2()), (h(), h2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(t, h), Cons(t2, h2)) => Some((Some(t()), Some(t2())), (h(), h2()))
      case (Cons(t, h), Empty) => Some((Some(t()), None), (h(), empty))
      case (Empty, Cons(t2, h2)) => Some((None, Some(t2())), (empty, h2()))
      case _ => None
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(empty))
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
  }



  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, t) => {
      if (p(h)) cons(h, t)
      else t
    })
  }

  def append[AA >: A](e: => Stream[AA]): Stream[AA] = {
    foldRight(e)((h, t) => cons[AA](h, t))
  }

  def map[B >: A](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).append(t))
  }

  def mapViaUnfold[B >: A](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    val zipped = zipAll(s).takeWhile {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

    //alternative
    //    zipped.forAll {
    //      case (a, b) => a.flatMap(av => b.map(bv => av == bv)).getOrElse(false)
    //    }

    zipped.toList2.length == s.toList2.length
  }

  def hasSubsequence[AA >: A](s: Stream[AA]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = cons[A](a, const)
    const
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1 + n2))
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => Empty
    }
  }
}