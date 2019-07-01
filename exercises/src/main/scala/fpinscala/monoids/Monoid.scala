package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  //10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1    
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  //10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None   
  }

  //10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = (x: A) => a2(a1(x))
    val zero = (x: A) => x
  }

  import fpinscala.testing._
  import Prop._

  //10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = 
    Prop.forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    )(x => m.op(m.op(x._1, x._2), x._3) == m.op(x._1, m.op(x._2, x._3))) &&
    Prop.forAll(gen)(a => m.op(m.zero, a) == a)

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  //10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  //10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, Monoid.endoMonoid[B])((x:A) => ((y:B) => f(x, y)))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  //10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) {
      m.zero
    }
    else if (as.length == 1) {
      f(as(0))
    } else {
      val (as1, as2) = as.splitAt(as.length / 2)
      val b1 = foldMapV(as1, m)(f)
      val b2 = foldMapV(as2, m)(f)
      m.op(b1, b2)
    }
  }

  //10.9
  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints zip ints.tail, booleanAnd){case (a: Int, b: Int) => a <= b}

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.op)
    val zero = Par.unit(m.zero)
  }    

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    if (v.length == 0) {
      Par.unit(m.zero)
    }
    else if (v.length == 1) {
      Par.unit(f(v(0)))
    } else {
      val (v1, v2) = v.splitAt(v.length / 2)
      val b1 = parFoldMap(v1, m)(f)
      val b2 = parFoldMap(v2, m)(f)
      b1.map2(b2)(m.op)
    }
  }

  //10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case(Stub(c1), Part(l1, words, r1)) => Part(c1 + l1, words, r1)
      case(Part(l1, words, r1), Stub(c1)) => Part(l1, words, r1 + c1)
      case(Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if(r1.length + r2.length > 0) 1 else 0), r2)
    }
    val zero = Stub("")
  }

  //10.11
  def count(s: String): Int = {
    def wc(c: Char): WC = {
      if(c == ' ')
        Part("", 0, "")
      else
        Stub(c.toString)
    }

    foldMap(s.toList, wcMonoid)(wc) match {
      case Stub(_) => 1
      case Part(l1, words, r1) => {
        words + (if(l1.length > 0) 1 else 0) + (if(r1.length > 0) 1 else 0)
      }
    }
  }

  //10.16
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)]{
    def op(a1: (A, B), a2: (A, B)) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    val zero = (a.zero, b.zero)
  }

  //10.17
  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B) = (a: A) => b.op(f1(a), f2(a))
    val zero: A => B = (a: A) => b.zero
  }  

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K,V]] {
    val zero = Map[K,V]()
    def op(a: Map[K,V], b: Map[K,V]) = 
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) => 
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  //10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = 
    foldMapV[A, Map[A, Int]](as, mapMergeMonoid(intAddition))(x => Map(x -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, b) => a :: b)
}

//10.12
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    //as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
    Monoid.foldMapV(as, mb)(f)
    
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

//10.13
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(la, ra) => mb.op(foldMap(la)(f)(mb), foldMap(ra)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a)  => f(z, a) 
    case Branch(la, ra) => foldLeft(ra)(foldLeft(la)(z)(f))(f)
  }
    
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a)  => f(a, z)
    case Branch(la, ra) => foldRight(la)(foldRight(ra)(z)(f))(f)
  }
}

//10.14
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }
    
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
    
}

