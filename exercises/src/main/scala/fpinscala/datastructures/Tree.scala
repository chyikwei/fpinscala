package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
    def size[A](t: Tree[A]): Int = {
        t match {
            case Leaf(_) => 1
            case Branch(l, r) => 1 + size(l) + size(r) 
        }
    }

    def maximum(t: Tree[Int]): Int = {
        t match {
            case Leaf(l) => l
            case Branch(l, r) => maximum(l) max maximum(r)
        }
    }

    def depth[A](t: Tree[A]): Int = {
        t match {
            case Leaf(l) => 1
            case Branch(l, r) => 1 + depth(l).max(depth(r))
        }
    }

    def map[A,B](t: Tree[A])(f: (A => B)): Tree[B] = {
        t match {
            case Leaf(l) => Leaf(f(l))
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        }
    }

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
        t match {
            case Leaf(l) => f(l)
            case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
        }
    }

    def size_by_fold[A](t: Tree[A]): Int = {
        fold(t)(_ => 1)(1 + _ + _)
    }

    def maximum_by_fold(t: Tree[Int]): Int = {
        fold(t)(x => x)(_ max _)
    }

    def depth_by_fold[A](t: Tree[A]): Int = {
        fold(t)(_ => 1)((l,r) => 1 + (l max r))
    }

    def map_by_fold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
        fold(t)(a => Leaf(f(a)): Tree[B])((l,r) => Branch(l,r))
    }
}