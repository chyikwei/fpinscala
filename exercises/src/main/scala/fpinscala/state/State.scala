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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt
    val aa = if (a < 0) -(a + 1) else a
    (aa, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    val b = a.toDouble / Int.MaxValue
    (b, rng2)
  }

  //6.5
  def double2: Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (a, rng2) = int(rng)
    val(b, rng3) = double(rng2)
    ((a, b), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (a, rng2) = int(rng)
    val(b, rng3) = double(rng2)
    ((b, a), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (a, rng2) = double(rng)
    val (b, rng3) = double(rng2)
    val (c, rng4) = double(rng3)
    ((a, b, c), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (Nil, rng)
    else {
      val (a, rng2) = int(rng)
      val (bb, rng3) = ints(count - 1)(rng2)
      (a :: bb, rng3)
    }
  }

  //6.6
  def map2[A,B,C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  //** 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit[List[A]](Nil))((f, acc) => map2(f)(acc)(_ :: _))
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  //6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  //6.9
  def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(x => unit(f(x)))
  }

  def map2FM[A,B,C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => (mapFM(rb)(b => f(a,b))))
  }

}

import State._
case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      (f(a), s1)
    }
  )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  //** 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  //**
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List[A]())) ((a, acc) => a.map2(acc)(_ :: _))
  }

  //get & set & modify
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // 6.11
  def update = {
    (input: Input) => (s: Machine) =>
    (input, s) match {
      case (_, Machine(_, 0, _)) => s
      // inset to unlocked machine
      case (Coin, Machine(false, _, _)) => s
      // turn locked machine
      case (Turn, Machine(true, _, _)) => s
      // insert to locked machine
      case (Coin, Machine(true, c, co)) => Machine(false, c, co+1)
      // turn unlocked machine
      case (Turn, Machine(false, c, co)) => Machine(true, c-1, co)
    }
  }
  // **6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify[Machine](update(i)(_))))
    s <- get
  } yield (s.candies, s.coins)

}

