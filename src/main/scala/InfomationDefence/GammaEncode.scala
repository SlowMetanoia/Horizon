package InfomationDefence

import scala.io.StdIn

object GammaEncode extends App {
  //Гамма
  class Gamma(key: LazyList[Int]){
    //кодирующая структура
    def codingStructure(text:String): Seq[(Char, Int)] = text.zip(key.take(text.length))
  }
  //шифрование
  def encode(gamma: Gamma,text:String):String =
    gamma
      .codingStructure(text)
      .map{ case (ch,key) => (key ^ ch).toChar }
      .mkString
  //расшифровывание
  def decode(gamma: Gamma,text:String):String =
    gamma
      .codingStructure(text)
      .map{ case (ch,key) => (key ^ ch).toChar }
      .mkString
  //ряд
  def series[T](prev:T,next:T=>T):LazyList[T] = prev#::series(next(prev),next)
  
  def gammaFromString(str:String):Gamma =
    new Gamma(
      series[String](str,_=>str)
        .flatten.map(_.toInt)
    )
  
  println("Input key:")
  val gamma = gammaFromString(StdIn.readLine)
  
  println("Input source text:")
  val sourceText = StdIn.readLine()
  
  println("encoded text:")
  val encoded = encode(gamma,sourceText)
  println(encoded)
  
  println("decoded text:")
  val decoded = decode(gamma,encoded)
  println(decoded)
}