package fpinscala.monoids

object MonoidTest {

    def main(args: Array[String]): Unit = {

        //10.11
        val c1 = Monoid.count("   aaa bbb xxx ccc ")
        assert(c1 == 4)
        val c2 = Monoid.count("a aa bbb xxx ccc ")
        assert(c2 == 5)
        val c3 = Monoid.count("  ")
        assert(c3 == 0)
        val c4 = Monoid.count("xxxx")
        assert(c4 == 1)

        //10.12
        val c5 = ListFoldable.foldMap(List(1,2,3))((x) => x + 1)(Monoid.intMultiplication)
        assert(c5 == 24)

        val c6 = IndexedSeqFoldable.foldMap(List(1,2,3).toIndexedSeq)((x) => x + 1)(Monoid.intMultiplication)
        assert(c5 == 24)

        //10.13
        val t1 = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
        val c7 = TreeFoldable.foldMap(t1)((x) => x + 2)(Monoid.intAddition)
        assert(c7 == 18)

        val c8 = TreeFoldable.foldLeft(t1)(0)((x, y) => x + y)
        assert(c8 == 10)

        val c9 = TreeFoldable.foldRight(t1)(0)((x, y) => x + y)
        assert(c9 == 10)

        //10.14
        val c10 = OptionFoldable.foldMap(Some(10))((x) => x)(Monoid.intMultiplication)
        assert(c10 == 10)

        //10.15
        val c11 = TreeFoldable.toList(t1)
        assert(c11 == List(1,2,3,4))

        //10.16
        val m1 = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)
        val c12 = m1.op((1,3), (2,3))
        assert(c12 == (3, 9))

        //10.17
        val m2 = Monoid.functionMonoid[Int, Int](Monoid.intAddition)
        val c14 = m2.op((x: Int) => x+1, (x:Int) => x +2)(10)
        assert(c14 == 23)
        
        val m3 = Monoid.functionMonoid[Int, Int](Monoid.intMultiplication)
        val c15 = m3.op((x: Int) => x+1, (x:Int) => x +2)(10)
        assert(c15 == 132)

        //10.18
        val c16 = Monoid.bag(List("aa", "bb", "cc", "aa", "bb").toIndexedSeq)
        //println(c16)
        assert(c16.size == 3)
        assert(c16.getOrElse("aa", -1) == 2)
        assert(c16.getOrElse("bb", -1) == 2)
        assert(c16.getOrElse("cc", -1) == 1)
    }
}
