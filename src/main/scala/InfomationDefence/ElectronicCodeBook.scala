package InfomationDefence

object ElectronicCodeBook extends App{
  //int2BinSrt - возьмём стандартное
  //binStr2Int - ...как обычно.
  def intFromBinString(str:String):Int =
    str
      .reverse
      .zipWithIndex
      .map{case (ch,i) => (ch-'0')*math.pow(2,i).toInt}
      .sum
      
  //64 bit костыльно.
  case class TextBlock(x:Int,y:Int){
    lazy val binString: String = x.toBinaryString + y.toBinaryString
  }
  
  case class Transposition(places:Seq[Int]){
    if(places.size != 64) throw new Exception("illegal number of positions")
    def apply[T](coll:Seq[T]):Seq[T] = places.map(i=>coll(i))
    def reverse:Transposition =
      Transposition(
        places
          .zipWithIndex
          .sortWith(_._1 > _._1)
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
}