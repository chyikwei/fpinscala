package fpinscala.laziness

object StreamTest {

  def main(args: Array[String]): Unit = {
    //5.1
    val s1 = Stream(1,2,3,4)
    val s2 = Stream(5,6)

    assert(s1.toList == List(1,2,3,4))

    //5.2
    assert(s1.take(2).toList == List(1,2))
    assert(s1.take(0).toList == Nil)

    assert(s1.drop(0).toList == List(1,2,3,4))
    assert(s1.drop(2).toList == List(3,4))
    assert(s1.drop(10).toList == Nil)

    //5.3
    assert(s1.takeWhile(_ < 3).toList == List(1,2))
    assert(s1.takeWhile(_ < 10).toList == List(1,2,3,4))
    assert(s1.takeWhile(_ > 10).toList == Nil)

    //5.4
    assert(s1.forAll(_ < 10))
    assert(s1.forAll(_ > 3) == false)

    //5.5
    assert(s1.takeWhileFR(_ < 3).toList == List(1,2))
    assert(s1.takeWhileFR(_ < 10).toList == List(1,2,3,4))
    assert(s1.takeWhileFR(_ > 10).toList == Nil)

    //5.6
    assert(s1.headOption == Some(1))
    assert(Empty.headOption == None)

    //5.7
    assert(s1.map(_ + 1).toList == List(2,3,4,5))
    assert(s1.filter(_ % 2 == 0).toList == List(2,4))
    assert(s1.append(s2).toList == List(1,2,3,4,5,6))
    assert(s2.flatMap(x => Stream(x,x)).toList == List(5,5,6,6))

    //5.8
    assert(Stream.constant(3).take(3).toList == List(3,3,3))

    //5.9
    assert(Stream.from(3).take(3).toList == List(3,4,5))

    //5.10   
    assert(Stream.fibs.take(7).toList == List(0,1,1,2,3,5,8))

    //5.11 & 5.12
    assert(Stream.fibsUF.take(7).toList == List(0,1,1,2,3,5,8))
    assert(Stream.constantUF(3).take(3).toList == List(3,3,3))
    assert(Stream.onesUF.take(3).toList == List(1,1,1))
    assert(Stream.fromUF(3).take(3).toList == List(3,4,5))

    //5.13
    assert(s1.mapUF(_ + 1).toList == List(2,3,4,5))
    assert(s1.takeUF(2).toList == List(1,2))
    assert(s1.takeWhileUF(_ < 3).toList == List(1,2))
    assert(s1.takeWhileUF(_ < 10).toList == List(1,2,3,4))
    assert(s1.takeWhileUF(_ > 10).toList == Nil)
    assert(s1.zipWith(Stream.ones)(_ + _).toList == List(2,3,4,5))
    assert(s1.take(2).zipAll(Stream.ones.take(3)).toList == 
        List((Some(1),Some(1)), (Some(2),Some(1)), (None,Some(1))))

    //5.14
    assert(s1.startsWith(Stream(1,2)))
    assert(s1.startsWith(Stream(1,4)) == false)
    assert(s1.startsWith(Stream(1,2,3,4,5)) == false)

    //5.15
    assert(s1.take(3).tails.map(_.toList).toList ==
        List(List(1, 2, 3), List(2, 3), List(3)))

    //5.16
    assert(s1.take(3).scanRight(0)(_ + _).toList == List(6,5,3,0))
  }
}