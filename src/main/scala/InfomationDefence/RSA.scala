package InfomationDefence

object RSA extends App{
  //ряды
  def series[T](prev:T)(next:T=>T):LazyList[T] = prev#::series(next(prev))(next)
  //решето Эратосфена
  def Erotosthenes(n:Long):Seq[Long] = {
    ???
  }
  def generatePrimary(n:Int):BigInt = {
  ???
  }
}
