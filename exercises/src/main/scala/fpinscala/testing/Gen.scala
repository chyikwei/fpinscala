package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

import language.postfixOps
import language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    
  //8.9  
  def && (p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
        case Falsified(f,s) => Falsified(f,s)
        case _ => p.run(m, n, rng)
    }
  }
  
  def || (p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
        case Falsified(_,_) => p.run(m, n, rng)
        case _ => Passed
    }    
  }

}


object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  /* copy from solution */
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
 
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)    

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  
  def check(p: => Boolean): Prop = Prop { (_, _, _) => 
    if (p) Proved else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  // from solution
  val S = Gen.weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  // 8.2
  //def &&(p: Prop): Prop = new Prop {
  //  def check = p.check && this.check
  //}

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map((x: A) => f(x)))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  //8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap((x: A) => f(x).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(x => Gen(State.sequence(List.fill(x)(sample))))
  }

  //8.10
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

}

object Gen {

  //8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(n => if (n % 2 == 0) true else false))
  }
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  //8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  //8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen.boolean.flatMap(x => if (x) g1 else g2)
  }

  //8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val prob1 = g1._2 / (g1._2 + g2._2)
    Gen(State(RNG.double).flatMap(d => if (d <= prob1) g1._1.sample else g2._1.sample))
  }

  //8.11
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(x => listOfN(x, g))

  //8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(x => listOfN(x max 1, g))  

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }
}


case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

}

