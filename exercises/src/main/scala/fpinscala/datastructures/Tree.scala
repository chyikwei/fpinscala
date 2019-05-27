package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(s) => s
    case Branch(l, r) => if(maximum(l) > maximum(r)) maximum(l) else maximum(r)
  }

  //3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => if(depth(l) > depth(r)) 1 + depth(l) else 1 + depth(r)
  }
  //3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(s) => f(s)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maximum2(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => 1 + (a max b))
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((a, b) => Branch(a, b))
}