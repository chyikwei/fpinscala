package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //5.1
  def toList: List[A] = {
    foldRight(Nil: List[A])((a, b) => a :: b.toList)
  }

  //5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => 
      if (n > 1) cons(h(), t().take(n-1))
      else if (n == 1) cons(h(), empty)
      else empty
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n-1) else this
    case _ => this
  }

  //5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case Cons(h, t) if (!p(h())) => empty
    case _ => empty
  }

  //5.4
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  //5.5
  def takeWhileFR(p: A => Boolean): Stream[A] = 
    foldRight(empty: Stream[A])((a, b) => if(p(a)) cons(a, b) else b)

  //5.6
  def headOption: Option[A] = 
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](p: A => B): Stream[B] = 
    foldRight(empty: Stream[B])((a, b) => cons(p(a), b))

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((a, b) => cons(a, b))
  
  def flatMap[B](p: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a, b) => p(a) append b)

  //5.13
  def mapUF[B](p: A => B): Stream[B] = {
    def helper(s: Stream[A]): Option[(B, Stream[A])] = s match {
      case Cons(h, t) => Some((p(h()), t()))
      case Empty => None 
    }
    unfold(this)(helper)
  }

  def takeUF(n: Int): Stream[A] = {
    def helper(s: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = s._1 match {
      case Cons(h, t) if s._2 > 0 => Some(h(), (t(), s._2-1))
      case _ => None 
    }
    unfold((this, n))(helper)
  }

  def takeWhileUF(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(t1, h1), Cons(t2, h2)) => Some((f(t1(), t2()), (h1(), h2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }
  }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean = {
    this.zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    } forAll {
      case (Some(x), Some(y)) => x == y
      case _ => false      
    }
  }

  //5.15
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some(cons(h(), t()), t())
      case _ => None
    }
  }

  //5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val c = acc
      val z1 = f(a, c._1)
      (z1, cons(z1, c._2))
    })._2
  }
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  //5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant[A](a))

  //5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  //5.10
  val fibs: Stream[Int] = {
    def helper(a1: Int, a2: Int): Stream[Int] = cons(a1, helper(a2, a1+a2))
    helper(0, 1)
  }

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, ss)) => cons(a, unfold(ss)(f))
    case None => empty
  }

  //5.12
  def fibsUF: Stream[Int] = {
    def helper(x: (Int, Int)): Option[(Int, (Int, Int))] = Some((x._1, (x._2, x._1 + x._2)))
    unfold[Int, (Int, Int)]((0,1))(helper)
  }

  def fromUF(n: Int): Stream[Int] = unfold[Int, Int](n)(x => Some(x, x+1))

  def constantUF[A](a: A): Stream[A] = unfold[A, A](a)(x => Some(x, x))

  val onesUF: Stream[Int] = unfold[Int, Int](1)(x => Some(x, x))

}
