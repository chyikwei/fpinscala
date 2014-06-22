package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
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
    val (i, rng2) = rng.nextInt
    val nn_i = if(i < 0) (i + 1).abs else i
    (nn_i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = rng.nextInt
    ( a.toDouble/Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i_1, rng2) = rng.nextInt
    val (i_2, rng3) = double(rng2)
    ((i_1, i_2), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i_1, rng2) = double(rng)
    val (i_2, rng3) = rng2.nextInt
    ((i_1, i_2), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (i_1, rng2) = double(rng)
    val (i_2, rng3) = double(rng2)
    val (i_3, rng4) = double(rng3)
    ((i_1, i_2, i_3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
      if(n <= 0)
        (acc, r)
      else {
        val (next_val, next_r) = r.nextInt
        loop(n-1, acc :+ next_val, next_r)
      }
    }
    loop(count, Nil, rng)
  }

  def positiveMax(n: Int): Rand[Int] = {
    map(nonNegativeInt)((x) => x % n)
  }

  def doubleByMap: Rand[Double] = {
    map(nonNegativeInt)((x) => x.toDouble/Int.MaxValue)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }
  
  def intDoubleMap(rng: RNG): Rand[(Int, Double)] = {
    map2(int, double)((_, _))
  }

  def doubleIntMap(rng: RNG): Rand[(Double, Int)] = {
    map2(double, int)((_, _))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil): Rand[List[A]])((r, acc) => map2(r, acc)(_ :: _))
  }

  def ints_seq(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(RNG.int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def map_fm[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(x => unit(f(x)))
  }

  def map2_fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  //this is hard...check solution...
  def map[B](f: A => B): State[S, B] = {
    State(
    s => {
      val (a, s1) = run(s)
      unit(f(a)).run(s1)
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(
      s => {
        val (a, s1) = run(s)
        val (b, s2) = sb.run(s1)
        unit(f(a,b)).run(s2)
      }
    )
  }
  
  //from solution
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit(Nil): State[S, List[A]])((r, acc) => r.map2(acc)(_ :: _))
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

//hard.
object State {
  type Rand[A] = State[RNG, A]

  //why???
  def _get[S]: State[S, S] = State(s => (s, s))
  def _set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}