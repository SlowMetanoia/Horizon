package InfomationDefence

object ECB extends App {
  //В качестве 64 бит у нас - Long.
  //Ну, если вдруг мы передаём не Long, то нам нужно привести к нему.
  class informationConverter[T](conversion:T=>Long){
  
  }
  //Туда
  def bStr2Long(str:String):Long =
    str.reverse.zipWithIndex.collect{
      case('1',i)=>math.pow(2,i).toLong
    }.sum
    
  //Сюда
  def long2BStr(l:Long):String = {
    val bStr = l.toBinaryString
    ("0"*(64-bStr.length))+bStr
  }
  
  //ПерестановОчка
  case class Transposition(places:Seq[Int]){
    if(places.size != 64) throw new IllegalArgumentException("Illegal size of collection!")
    def apply[T](coll:Seq[T]):Seq[T] = places.map(i => coll(i))
    //перестановка, обращающая эту.
    def reverse:Transposition =
      Transposition(
        places
          .zipWithIndex
          .sortWith(_._1 < _._1)
          .map(_._2)
        )
  }
  
  val trsp = Transposition(Seq(57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7, 56, 48, 40, 32, 24, 16, 8, 0, 58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14,6))
  
  
  def series[T](prev:T,next:T=>T):LazyList[T] = prev#::series(prev,next)
  
  val someValue = 1924385729384573423L
  println(s"someValue = ${ someValue }")
  val svBits = long2BStr(someValue)
  println(s"svBits = ${ svBits }")
  val svTBits = trsp(svBits).mkString
  println(s"svTBits = ${ svTBits }")
  val svT = bStr2Long(svTBits)
  println(s"svT = ${ svT }")
  val svTRBits = trsp.reverse(svTBits).mkString
  println(s"svTRBits = ${ svTRBits }")
  val svTR = bStr2Long(svTRBits)
  println(s"svTR = ${ svTR }")
  
  println
  println("YEY!")
}
