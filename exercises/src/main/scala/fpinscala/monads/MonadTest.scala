package fpinscala.monads

object MonadTest {

    def main(args: Array[String]): Unit = {
        val c = Functor.listFunctor.map(List(1,2,3))(_ + 1)
        println(c)

        val d = Monad.listMonad.flatMap(List(1,2,3))(x => List(x + 1))
        println(d)
    }
}