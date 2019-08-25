package fpinscala.streamingio

object StreamingIOTest {

    def main(args: Array[String]): Unit = {
        import SimpleStreamTransducers.Process._
        val p = liftOne((x: Int) => x * 2)
        val xs = p(Stream(1,2,3)).toList
        println(xs)

        val even = filter((x: Int) => x % 2 == 0)
        val evens = even(Stream(1,2,3,4)).toList
        println(evens)

        val s = sum(Stream(1.0,2.0,3.0,4.0)).toList
        println(s)

        val counts = count(Stream(1,2,3,4)).toList
        println(counts)

    }
}