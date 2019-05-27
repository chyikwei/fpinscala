package fpinscala.datastructures

object TreeTest {

  def main(args: Array[String]): Unit = {
    val t1 = Leaf(1)
    val t2 = Leaf(3)
    val t3 = Branch(t1, t2)
    val t4 = Branch(t3, t1)

    // 3.25
    assert(Tree.size(t1) == 1)
    assert(Tree.size(t3) == 3)
    assert(Tree.size(t4) == 5)
    //3.26
    assert(Tree.maximum(t3) == 3)
    assert(Tree.maximum(t4) == 3)
    // 3.27
    assert(Tree.depth(t1) == 1)
    assert(Tree.depth(t3) == 2)
    assert(Tree.depth(t4) == 3)
    //3.28
    assert(Tree.map(t4)(_ + 1) == Branch(Branch(Leaf(2), Leaf(4)), Leaf(2)))
    //3.29
    assert(Tree.size2(t1) == 1)
    assert(Tree.size2(t3) == 3)
    assert(Tree.size2(t4) == 5)

    assert(Tree.maximum2(t3) == 3)
    assert(Tree.maximum2(t4) == 3)

    assert(Tree.depth2(t1) == 1)
    assert(Tree.depth2(t3) == 2)
    assert(Tree.depth2(t4) == 3)

    assert(Tree.map2(t4)(_ + 1) == Branch(Branch(Leaf(2), Leaf(4)), Leaf(2)))
  }
}
