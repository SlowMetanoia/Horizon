package InfomationDefence

object ElectronicCodeBook extends App{
  /**
   * Таки пришлось писать битмап.
   * @param bytes исходные байты
   */
  case class BitMap(bytes:Byte*) extends Iterable[Boolean] {
    //ряды
    def series[T](prev:T,next:T=>T):LazyList[T] = prev#::series(prev,next)
    //def this(n:Int) = this(series[Byte](0,_=>0).take(n):_*)
    
    //def this(bitSet: mutable.BitSet) = this(???)
  
    override def iterator: Iterator[ Boolean ] = ???
  }
  
  //int2BinSrt - возьмём стандартное и дополним до 32 знаков
  def int2BinStr(n:Int) = {
    val binStr = n.toBinaryString
    val result = "0"*(32-binStr.length) ++ binStr
    if(result.length != 32) throw new Exception("illegal binStr length")
    result
  }
  //binStr2Int - ...как обычно.
  def binString2Int(str:String):Int =
    str
      .reverse
      .zipWithIndex
      .map{case (ch,i) => (ch-'0')*math.pow(2,i).toInt}
      .sum

  //64 bit костыльно.
  case class TextBlock(x:Int,y:Int){
    def binString: String = {
      val bs = int2BinStr(x) + int2BinStr(y)
      if(bs.length !=64) throw new Exception("illegal text block lLength")
      bs
    }
    def transposedBitsString(trp:Transposition):String = trp(binString).mkString
  }
  object TextBlock{
    def apply(str:String): TextBlock = TextBlock(binString2Int(str.take(32)), binString2Int(str.drop(32)))
  }
  
  case class Transposition(places:Seq[Int]){
    if(places.size != 64) throw new Exception("illegal number of positions")
    def apply[T](coll:Seq[T]):Seq[T] = {
      if (coll.length != 64) throw new Exception("illegal number of positions in transponsing collection")
      places.map(i => coll(i))
    }
    def reverse:Transposition =
      Transposition(
        places
          .zipWithIndex
          .sortWith(_._1 < _._1)
          .map(_._2)
        )
  }
  
  def wikiTable2Transposition(table:String):Transposition =
    Transposition(
      table
        .replace('	',' ')
        .replace('\n',' ')
        .split(' ')
        .collect{ case str:String if str.nonEmpty => str.toInt - 1}
        .toSeq
      )


  val transposition = //Transposition((32 to 63) ++ (0 to 31))
    Transposition(Seq(57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7, 56, 48, 40, 32, 24, 16, 8, 0, 58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14,6))
  val reversed = transposition.reverse
  
  val transpositionCheck = transposition.reverse(transposition(0 to 63))
  println(transpositionCheck==(0 to 63))
  val textBlockCreationCheck = ???
  
  val tb = TextBlock(11283,32409)
  
  val transposed = TextBlock(tb.transposedBitsString(transposition))
  
  val reverted = TextBlock(transposed.transposedBitsString(reversed))
  //TextBlock(tb.binString)
  println(tb)
  println(transposed)
  println(reverted)
}