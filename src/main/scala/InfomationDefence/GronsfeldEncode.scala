package InfomationDefence

import scala.io.StdIn


object GronsfeldEncode extends App{
  //Ключ
  class CodingKey(key:String){
    //бесконечная последовательность значений равных ключу
    def repeat:LazyList[String] = key#::repeat
    //Бесконечно повторяющаяся последовательность символов ключа
    def fullKey:LazyList[Char] = repeat.flatten
    //результат сцепления соответствующих элементов последовательности
    def codingStructure(text:String): Seq[(Char, Char)] = text.zip(fullKey.take(text.length))
  }
  //закодировать текст
  def encode(key: CodingKey, text:String): String =
    key.codingStructure(text).map{case (symbol,shift) => (symbol+shift).toChar}.mkString
  //декодировать текст
  def decode(key: CodingKey, text:String): String =
    key.codingStructure(text).map{case (symbol,shift) => (symbol-shift).toChar}.mkString



  println("Input key:")
  val key = new CodingKey(StdIn.readLine)

  println("Input source text:")
  val sourceText = StdIn.readLine()

  println("encoded text:")
  val encoded = encode(key,sourceText)
  println(encoded)

  println("decoded text:")
  val decoded = decode(key,encoded)
  println(decoded)
}
