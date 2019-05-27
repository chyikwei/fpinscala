package fpinscala.errorhandling

object EitherTest {

  def main(args: Array[String]): Unit = {
    // 4.6
    val a1: Either[String, Int] = Left("xxx")
    val a2: Either[String, Int] = Right(3)
    val a3: Either[String, Int] = Right(7)

    assert(a2.map(_ + 1) == Right(4))
    assert(a1.map(_ + 1) == Left("xxx"))

    assert(a2.flatMap((x) => Right(x + 3)) == Right(6))
    assert(a1.flatMap((x) => Right(x + 3)) == Left("xxx"))

    assert(a1.orElse(Right(7)) == Right(7))
    assert(a2.map2(a3)(_ + _) == Right(10))

    //4.7
    val a4 = List(1,2,3,4)
    assert(Either.traverse(a4)((x) => if (x > 1) Right(x) else a1) == a1)
    assert(Either.traverse(a4)((x) => if (x > 0) Right(x) else a1) == Right(a4))

    assert(Either.sequence(List(a1, a2, a3)) == a1)
    assert(Either.sequence(List(a2, a3)) == Right(List(3,7)))

  }
}