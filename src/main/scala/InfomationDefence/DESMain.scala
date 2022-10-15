package InfomationDefence


import scala.io.StdIn
import scala.util.{Success, Try}

object DESMain extends App{
  /*
  //Для дебага
  val des = new DES("useless")
  val x = 345678945674567L
  val encoded = des.encodeBlock(x)
  val decoded = des.decodeBlock(encoded)
  println(s"x       = $x")
  println(s"encoded = $encoded")
  println(s"decoded = $decoded")
  */

  def str2Longs(text:String):Seq[Long] = text.grouped(4).map(cc=> cc.zipWithIndex.map{
    case (e,i) => e * math.pow(Char.MaxValue,i).toLong
  }.sum).toSeq

  def longs2Str(longs:Seq[Long]):String =
    longs
      .flatMap(x=>
        for(i<- 0 to 3) yield x/math.pow(Char.MaxValue,i).toLong % Char.MaxValue)
      .collect{
        case ch if ch != 0 => ch.toChar
      }
      .mkString

  println("Input initString:")
  val initString = StdIn.readLine()

  //Экземпляр шифровальщика
  val desInstance = DES(initString)

  //IV для OFB
  val initVector: Long = {
    println("Input init vector, it must be Long (64 bit, signed) value:")
    var iv = Try(StdIn.readLong())
    while (iv.isFailure){
      println("wrong! try again!")
      iv = Try(StdIn.readLong())
    }
    iv.get
  }


  println("Input source text:")
  val sourceText = StdIn.readLine()

  println("ECB encoded text:")
  val ecbEncoded = desInstance.ECBEncode(str2Longs,Seq(sourceText))
  println(ecbEncoded)

  println("OFB encoded text:")
  val ofbEncoded = desInstance.OFBEncode(str2Longs,initVector,Seq(sourceText))
  println(ofbEncoded)

  println("decoded ECB text:")
  val ecbDecoded = desInstance.ECBDecode(longs2Str,ecbEncoded)
  println(ecbDecoded.mkString)

  println("decoded OFB text:")
  val ofbDecoded = desInstance.OFBDecode(longs2Str,ofbEncoded,initVector)
  println(ofbDecoded.mkString)
}
