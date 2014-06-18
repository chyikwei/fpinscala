package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toListRec: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }
  }

  def toList: List[A] = {
    def loop(acc: List[A], rest: Stream[A]): List[A] = {
      rest match {
        case Cons(h, t) => loop(acc :+ h(), t())
        case _ => acc
      }
    }
    loop(Nil, this)
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    if(n <= 0) Empty
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => cons(h(), t().take(n-1))
      }
  }

  def drop(n: Int): Stream[A] = {
    if(n <= 0) this
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n-1)
      }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => if(p(h())) cons(h(), t().takeWhile(p)) else t().takeWhile(p)
    }
  }

  def takeWhileWithFL(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) ((a, b) => if(p(a)) cons(a, b) else Empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B]) ((a, b) => cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) ((a,b) => if(f(a)) cons(a, b) else b)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s) ((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B]) ((a, b) => f(a).append(b))
  }

  // implement with unfold
  def mapByUF[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def takeWhileByUF(p: A => Boolean): Stream[A] = {
    unfold (this) {
      case Cons(h, t) => if(p(h())) Some((h(), t())) else None
      case Empty => None
    }
  }

  def takeByUF(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h, t), n) => if(n > 0) Some((h(), (t(), n-1))) else None
      case _ => None
    }
  }

  def zipByUF[B](s2: Stream[B]): Stream[(A,B)] = {
    unfold ((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  //from answers
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def zipAllByUF[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    val ss1 = this map(Some(_)) append constant(None)
    val ss2 = s2 map(Some(_)) append constant(None)
    unfold((ss1, ss2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => {
        val head_1 = h1()
        val head_2 = h2()
        (head_1, head_2) match {
          case (None, None) => None
          case _ => Some(((head_1,head_2), (t1(), t2())))
        }
      }
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    def func(x: (Option[B], Option[B])) = !x._2.isEmpty
    this.zipAllByUF(s).takeWhile(func).forAll{case (h1, h2) => h1 == h2}
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((this, t()))
    } append(Empty)
  }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    s1.tails exists (_ startsWith(s2))
  }

  // this is hard
  //def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = 
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

  def fibs: Stream[Int] = {
    def loop(prev_2: Int, prev_1: Int): Stream[Int] = {
      cons(prev_2, loop(prev_1, prev_1 + prev_2))
    }
    loop(0, 1)
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fibsByUF: Stream[Int] = {
    def f(a: (Int, Int)) = Some((a._1, (a._2, a._1 + a._2)))
    unfold((0, 1))(f)
  }

  def fromByUF(n: Int): Stream[Int] = {
    unfold(n)((a) => Some((a, a+1)))
  }

  def constantByUF[A](a: A): Stream[A] = {
    unfold(a) ((a) => Some((a, a)))
  }

}