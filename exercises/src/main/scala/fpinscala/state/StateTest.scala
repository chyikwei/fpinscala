package fpinscala.state

object StateTest {

    def main(args: Array[String]): Unit = {
        val seed = 100
        val s = RNG.Simple(seed)
        
        //6.1
        val (a1, s2) = RNG.nonNegativeInt(s)
        val (a2, s3) = RNG.nonNegativeInt(s2)
        println(a1, a2)

        //6.2
        val (d1, s4) = RNG.double(s3)
        val (d2, s5) = RNG.double(s4)
        println(d1, d2)

        //6.3
        val (dd1, s6) = RNG.intDouble(s5)
        val (dd2, s7) = RNG.doubleInt(s6)
        val (dd3, s8) = RNG.double3(s7)
        println(dd1, dd2, dd3)

        //6.4
        val (i5, s9) = RNG.ints(5)(s8)
        println(i5)

        //6.5
        val (d3, s10) = RNG.double2(s9)
        println(d3)

        //6.6
        val dSum = RNG.map2(RNG.double)(RNG.double)(_ + _)
        val (d4, s11) = dSum(s10)
        println(d4)

        //6.7
        val (i6, s12) = RNG.ints2(5)(s11)
        println(i6)

        //6.8 & 6.9
        val (d5, s13) = RNG.mapFM(RNG.double)(_ + 3)(s12)
        println(d5)
        val dSumFM = RNG.map2FM(RNG.double)(RNG.double)(_ + _)
        val (d6, s14) = dSumFM(s13)
        println(d6)

        //6.11
        val m = Machine(true, 10, 0)
        val inputs = List(Coin, Turn, Coin, Turn, Turn, Turn, Coin, Coin, Turn)
        val (state, m1) = State.simulateMachine(inputs).run(m)
        println(state)
    }
}