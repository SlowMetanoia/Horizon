package InfomationDefence



object GronsfeldEncode extends App{
  class CodingKey(key:String){
    def repeat(key:String):LazyList[String] = key#::repeat(key)
    def fullKey(key:String):LazyList[Char] = repeat(key).flatten
    def codingStructure(text:String): Seq[(Char, Char)] = text.zip(fullKey(key).take(text.length))
  }
  def encode(alfaBet: CodingKey,text:String): String =
    alfaBet.codingStructure(text).map{case (symbol,shift) => (symbol+shift).toChar}.mkString
  
  def decode(alfaBet: CodingKey,text:String): String =
    alfaBet.codingStructure(text).map{case (symbol,shift) => (symbol-shift).toChar}.mkString
    
  val codingKey = new CodingKey("123456783456345")
  
  val text = "Not useless murderer"
  
  println(text)
  
  val encoded = encode(codingKey,text)
  
  println(encoded)
  
  val decoded = decode(codingKey,encoded)
  
  println(decoded)
  
}
