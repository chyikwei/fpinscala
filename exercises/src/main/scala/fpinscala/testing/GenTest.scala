package fpinscala.testing

import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import java.util.concurrent.{Executors,ExecutorService}

object GenTest {

    def main(args: Array[String]): Unit = {
        val seed = 100
        val s = RNG.Simple(seed)

        //8.4
        val g1 = Gen.choose(10, 20)
        val (r1, s1) = g1.sample.run(s)
        assert(r1 >= 10)
        assert(r1 < 20)

        //8.5
        val g2 = Gen.unit(1)
        val (r2, s2) = g2.sample.run(s1)
        assert(r2 == 1)

        val g3 = Gen.listOfN(20, g1)
        val (r3, s3) = g3.sample.run(s2)
        assert(r3.forall(x => x >= 10 && x < 20))

        //8.6
        val g4 = Gen.unit(3).flatMap((x: Int) => Gen.unit(x.toString + "!!"))
        val (r4, s4) = g4.sample.run(s3)
        assert(r4 == "3!!")

        val g5 = g1.listOfN(Gen.unit(5))
        val (r5, s5) = g5.sample.run(s4)
        assert(r5.length == 5)
        assert(r5.forall(x => x >= 10 && x < 20))

        //8.7
        val g6 = Gen.union(Gen.unit(1), Gen.unit(2))
        val (r6, s6) = g6.sample.run(s5)
        assert(r6 == 1 || r6 == 2)
        
        //8.8
        val g7 = Gen.weighted((Gen.unit(1), 0.3), (Gen.unit(2), 0.4))
        val (r7, s7) = g7.sample.run(s6)
        assert(r7 == 1 || r7 == 2)

        val g8 = Gen.weighted((Gen.unit(1), 0.1), (Gen.unit(2), 0.0))
        val (r8, s8) = g8.sample.run(s7)
        assert(r8 == 1)

        // 8.9
        val r9 = Prop.forAll(g8)(x => x > 0).run(10, 10, s8)
        assert(r9 == Prop.Passed)

        val r10 = Prop.forAll(g5)(x => x.forall(y => y >= 10 && y < 20)).run(10, 10, s8)
        assert(!r10.isFalsified)

        val r11 = Prop.forAll(g5)(x => x.forall(y => y < 10)).run(10, 10, s8)
        assert(r11.isFalsified)

        // 8.10 & 8.12
        val sg1 = Gen.listOf(g1)
        val r12 = Prop.forAll(sg1)(x => x.forall(y => y >= 10 && y < 20)).run(10, 10, s8)

        // example
        val smallInt = Gen.choose(-10, 10)
        val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns => 
            val max = ns.max
            !ns.exists(_ > max)
        }
        Prop.run(maxProp)

        //8.14
        val sortedProp = Prop.forAll(Gen.listOf1(smallInt)) { ns => 
            val s = ns.sorted
            (s zip s.tail).forall {case (a1, a2) => a1 <= a2}
        }
        Prop.run(sortedProp)

        // example
        val ES: ExecutorService = Executors.newCachedThreadPool
        val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
        Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
        Prop.run(p1)

        //example
        Prop.run(Prop.check(true))

        val p2 = Prop.check {
          val p = Par.map(Par.unit(1))(_ + 1)
          val p2 = Par.unit(2)
          p(ES).get == p2(ES).get
        }
        Prop.run(p2)

        val p3 = Prop.check {
          Prop.equal (
            Par.map(Par.unit(1))(_ + 1),
            Par.unit(2)
          ) (ES) get
        }
        Prop.run(p3)

        val pint = Gen.choose(0,10) map (Par.unit(_))
        val p4 = Prop.forAllPar(pint)(n => Prop.equal(Par.map(n)(y => y), n))
        Prop.run(p4)

        //8.17 from solution but the fork
        //lazy val pint2: Gen[Par[Int]] = Gen.choose(-100,100).listOfN(Gen.choose(0,20)).map(l =>
        //    l.foldLeft(Par.unit(0))((p,i) =>
        //    Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

        //val forkProp = Prop.forAllPar(pint2)(i => Prop.equal(Par.fork(i), i))
        //Prop.run(forkProp)

    }
}