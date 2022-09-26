package InfomationDefence

object GammaEncode extends App {
  class Gamma(key: LazyList[Int]){
    def codingStructure(text:String): Seq[(Char, Int)] = text.zip(key.take(text.length))
  }
  def encode(gamma: Gamma,text:String):String =
    gamma.codingStructure(text).map{ case (ch,key) => (key ^ ch).toChar }.mkString
    
  def decode(gamma: Gamma,text:String):String =
    gamma.codingStructure(text).map{ case (ch,key) => (key ^ ch).toChar }.mkString
  
  def series(prev:Int,next:Int=>Int):LazyList[Int] = prev#::series(next(prev),next)
  
  val N = series(1,_+1)
  
  val gamma = new Gamma(N)
  
  val text = "Useless murderer"
  
  println(text)
  
  val encoded = encode(gamma,text)
  
  println(encoded)
  
  val decoded = decode(gamma,encoded)
  
  println(decoded)
}
