package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else {
      l match {
        case Cons(x, xs) => drop(xs, n-1)
        case _ => Nil
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) {
          dropWhile(xs, f)
        } else {
          Cons(x, xs)
        }
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, total) => total + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((total, _) => total + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc, element) => Cons(element, acc))
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  def append2[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)((element, acc) => Cons(element, acc))

  //3.15
  def concatList[A](a1: List[List[A]]): List[A] = 
      foldLeft(a1, Nil: List[A])(append2)
  
  //3.16
  def addOne(l : List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, addOne(xs))
    }
  }

  //3.17
  def doubleToStr(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, doubleToStr(xs))
    }
  }

  //3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) {
          Cons(x, filter(xs)(f))          
        } else {
          filter(xs)(f)
        }
      }
    }
  }

  //3.20
  def flatMap[A, B] (as: List[A])(f: A => List[B]): List[B] = 
    concatList(map(as)(f))

  //3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)((x: A) => if(f(x)) List(x) else Nil)

  //3.22
  def listAdd(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, xs) => Nil
      case (xs, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, listAdd(xs1, xs2))
    }
  }

  //3.23
  def zipWith[A,B,C](a1: List[A], b1: List[B])(f: (A, B) => C): List[C] = {
    (a1, b1) match {
      case (Nil, xs) => Nil
      case (xs, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }
  //3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => 
        if (x == y)
          hasSubsequence(xs, ys) || hasSubsequence(xs, sub)
        else 
          hasSubsequence(xs, sub)
    }
  }
}