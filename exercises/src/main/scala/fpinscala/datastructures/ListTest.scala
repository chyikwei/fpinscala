package fpinscala.datastructures

object ListTest {

    def main(args: Array[String]): Unit = {
        assert(List.x == 3)

        val t1 = List(1,2,3,4)
        assert(List.tail(t1) == List(2,3,4))
        assert(List.setHead(t1, 5) == List(5,2,3,4))
        assert(List.drop(t1, 2) == List(3,4))
        assert(List.dropWhile(t1, (x: Int) => x < 4) == List(4))
        assert(List.init(t1) == List(1,2,3))
        assert(List.length(t1) == 4)

        val t2 = List.foldRight(t1, Nil : List[Int])(Cons(_, _))
        assert(t2 == t1)

        assert(List.foldLeft(t1, 1)(_ + _) == 11)
        assert(List.sum3(t1) == List.sum2(t1))

        val t3 = List(1.0, 2.0, 3.0, 4.0)
        assert(List.product3(t3) == List.product2(t3))
        assert(List.length2(t1) == 4)
        assert(List.reverse(t1) == List(4,3,2,1))
        val t4 = List(5,6)
        assert(List.append2(t1, t4) == List(1,2,3,4,5,6))

        val t5 = List(t1, t4, List(7,8))
        assert(List.concatList(t5) == List(1,2,3,4,5,6,7,8))

        assert(List.addOne(List(1,2,3,4)) == List(2,3,4,5))

        assert(List.doubleToStr(t3) == List("1.0", "2.0", "3.0", "4.0"))

        assert(List.map(t3)(_.toString) == List("1.0", "2.0", "3.0", "4.0"))

        assert(List.filter(t1)(_ % 2 == 0) == List(2,4))

        assert(List.flatMap(List(1,2,3))(i => List(i, i)) == List(1,1,2,2,3,3))
        assert(List.filter2(t1)(_ % 2 == 0) == List(2,4))        

        assert(List.listAdd(List(1,2,3), List(4,5,6)) == List(5,7,9))
        assert(List.zipWith(List(1,2,3), List(4,5,6))(_ + _) == List(5,7,9))

        assert(List.hasSubsequence(List(1,2,3,4), List(3,4)))
        assert(List.hasSubsequence(List(1,2,3), List(2,4)) == false)
  }

}
