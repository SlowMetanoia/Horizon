/**
 * DES
 */

def series[T](prev:T)(next:T=>T):LazyList[T] = prev#::series(next(prev))(next)

val longs = series[Long](1)(_*2).take(64).force
longs.take(64).force.mkString("\n")

Seq(10).take(10)