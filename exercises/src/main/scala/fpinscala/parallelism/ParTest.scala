package fpinscala.parallelism

import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.TimeUnit

object ParTest {
    import Par._

    def main(args: Array[String]): Unit = {
        // init
        val es = Executors.newFixedThreadPool(10)
        try {

            val p1 = Par.unit(3)
            val r1 = p1(es).get()
            assert(r1 == 3)

            // will cause deadlock the number is too large
            val p2 = (1 to 2).toIndexedSeq
            val r2 = Examples.sum(p2)(es).get(3L, TimeUnit.SECONDS)
            assert(r2 == 3)
            
            //test fork
            val p3 = Par.fork(addOne(3))
            val r3 = p3(es).get()
            assert(r3 == 4)

            //7.4
            val p4 = asyncF((x:Int) => x.toString + "!!")
            val r4 = p4(3)(es).get()
            assert(r4 == "3!!")

            val p5 = map(p1)((x: Int) => x.toString + "??")
            val r5 = p5(es).get()
            assert(r5 == "3??")

            //7.5
            val p6 = List(Par.unit(1), Par.unit(2))
            val r6 = sequence(p6)(es).get()
            assert(r6 == List(1,2))

            val p7 = parMap(List(1,2))(_ + 1)
            val r7 = p7(es).get()
            assert(r7 == List(2,3))

            //7.6
            val p8 = parFilter(List(1, 2))(_ % 2 == 0)
            val r8 = p8(es).get()
            assert(r8 == List(2))

            //7.11
            val p9 = choiceN(Par.unit(1))(List(Par.unit(2), Par.unit(3), Par.unit(4)))
            val r9 = p9(es).get()
            assert(r9 == 3)

            val p10 = choice2(Par.unit(true))(Par.unit(3), Par.unit(4))
            val r10 = p10(es).get()
            assert(r10 == 3)

            //7.12
            val p11 = choiceMap(unit(2))(Map(1 -> unit(2), 2 -> unit(3)))
            val r11 = p11(es).get()
            assert(r11 == 3)

            //7.13
            val p12 = chooser(unit(2))((x: Int) => if (x==1) unit(3) else unit(4))
            val r12 = p12(es).get()
            assert(r12 == 4)

            //7.14
            val p13 = flatMap(unit(1))(x => unit(x+1))
            val r13 = p13(es).get()
            assert(r13 == 2)

            val p14 = join(unit(unit(1)))
            val r14 = p14(es).get()
            assert(r14 == 1)

            val p15 = flatMapByJ(unit(1))(x => unit(x+1))
            val r15 = p15(es).get()
            assert(r15 == 2)

            val p16 = joinByFM(unit(unit(1)))
            val r16 = p16(es).get()
            assert(r16 == 1)

        }
        finally {
            es.shutdown();    
        }
    }

    def addOne(i: Int): Par[Int] = Par.unit(i + 1)

}