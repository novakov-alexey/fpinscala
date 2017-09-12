package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { i => unit(f(i)) }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    if (i < 0)
      nonNegativeInt(nextRng)
    else
      (i, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }

  def double2(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Integer.MAX_VALUE.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = nonNegativeInt(rng1)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(acc: List[Int], count: Int, nextRng: RNG): (List[Int], RNG) = {
      val (i, r) = nextRng.nextInt
      if (count > 0)
        loop(acc :+ i, count - 1, r)
      else (acc, r)
    }

    loop(List(), count, rng)
  }

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1): (A, RNG) = ra(rng)
      val (b, rng2): (B, RNG) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb) { b => f(a, b) }
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((f, acc) =>
      map2(f, acc)(_ :: _)
    )
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => (f(run), s))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def loop(s: S, acc: List[A], actions: List[State[S, A]]): (List[A], S) = {
      actions match {
        case Nil => (acc, s)
        case x :: xs =>
          val (a, s1) = x.run(s)
          loop(s1, acc :+ a, xs)
      }
    }

    State(s => loop(s, List(), fs))
  }
}
