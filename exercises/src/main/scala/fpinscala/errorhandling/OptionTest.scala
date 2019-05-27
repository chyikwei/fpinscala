package fpinscala.errorhandling

object OptionTest {

  def main(args: Array[String]): Unit = {
    assert(Some(3).map(_ + 1).getOrElse(-1) == 4)
    assert(None.getOrElse(-1) == -1)
    
    //4.2
    val s1 = Seq(1.0, 2.0, 3.0)
    val v1 = Option.variance(s1).getOrElse(1.0)
    assert(math.abs(v1 - 0.67) < 0.01)

    //4.3
    assert(Option.map2(Some(1), Some(2))(_ + _).getOrElse(-1) == 3)
    assert(Option.map2(Some(1), None)(_ + _).getOrElse(-1) == -1)
    assert(Option.map2(None: Option[Int], None)(_ + _).getOrElse(-1) == -1)

    //4.4
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3)))
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
    assert(Option.sequence(List(None)) == None)

    //4.5
    val s2 = List(1,2,3)
    assert(Option.traverse(s2)((x) => if(x > 0) Some(x+1) else None) == Some(List(2,3,4)))

    assert(Option.sequence2(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3)))
    assert(Option.sequence2(List(Some(1), None, Some(3))) == None)
    assert(Option.sequence2(List(None)) == None)

  }
}