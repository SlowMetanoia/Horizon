package InfomationDefence

object ECB extends App {
  //В качестве 64 бит у нас - Long.
  //Ну, если вдруг мы передаём не Long, то нам нужно привести к нему.
  class informationConverter[T](conversion:T=>Long)
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
    def apply[T](coll:Seq[T]):Seq[T] = {
      if(coll.length!=places.length)
        throw new IllegalArgumentException("Collection must be the same length as transposition is")
      places.map(i => coll(i))
    }
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
  
  //Плохо ли это? - Да!
  def leftCycleTransposition(l:Long,bitLength:Int) = {
    val bStr = long2BStr(l).takeRight(bitLength)
    bStr2Long(bStr.tail + bStr.head)
  }
  
  /**
   * Преобразования сетью Фейстеля
   * @param l исходные данные
   * @param startKey исходный шифровальный ключ, 7 byte
   * @param k количество итераций
   */
  def feistelTransformation( l:Long, startKey:Seq[Byte], k:Int = 16): Unit = {
    val extensionTable = Seq(32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9, 8, 9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17, 16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25, 24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1)
    //эмуляция последовательного сцепления байт в int
    def bytes2Int(bytes:Seq[Byte]):Int = bytes
      .zipWithIndex
      .map{case (n,i)=> n*math.pow(256,i).toInt}
      .sum
    
    val key = bytes2Int(startKey)
    //Разделение на 2 части по 32 бита
    def partition(l:Long):(Int,Int) = {
      long2BStr(l)
        .grouped(32)
        .map(bStr2Long(_).toInt)
        .toSeq
      match {case Seq(l,r) => (l,r)}
    }
    val (left,right) = partition(l)
    def feistelFunction( r:Int, key:Long):Int = {
      
      def extension(r:Int,extensionTable:Seq[Int]):Long = {
        val bitStr = long2BStr(r)
        bStr2Long(extensionTable.map(bitStr).mkString)
      }
      
      //странное преобразование в блоки по 6 бит
      def extended48BitTo6BitBlocks(l:Long):Seq[Byte] = long2BStr(l)
        .reverse
        .grouped(6)
        .take(8)
        .toSeq
        .reverse
        .map(bStr2Long(_).toByte)
      //преобразование блока из 6 бит к блоку из 4 бит
      def tableConversion(value:Byte,table:Seq[Seq[Byte]]):Byte = {
        val valBits = value.toBinaryString
        val allBits = "0"*(6-valBits.length) + valBits
        val a = bStr2Long(Seq(allBits.head, allBits.last).mkString).toInt
        val b = bStr2Long(allBits.drop(1).dropRight(1)).toInt
        table(a)(b)
      }
      //Сложение ключа с исходными данными по модулю 2.
      def applyKey:(Long,Long)=>Long = _^_
      val c = extension(_,extensionTable)
      c
        .andThen(applyKey(_,key))
        .andThen(extended48BitTo6BitBlocks)
      ???
    }
    def transform(l:Int,r:Int,key:Long):(Int,Int,Long) =
      (r,l^feistelFunction(r,key),key)
    
  }
  
  
  /*
  //Это дебаг к leftCycleTransposition
  val input = 285435L
  val bInput = long2BStr(input)
  val lct = leftCycleTransposition(input,32)
  val blct = long2BStr(lct)
  println(s"input = ${ input }")
  println(s"bInput = ${ bInput }")
  println(s"lct = ${ lct }")
  println(s"blct   = ${ blct }")
  */
  
  
  
  /*
  //Это дебаг к перестановкам и переводу туда-сюда
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
  */
  println
  println("YEY!")
}
