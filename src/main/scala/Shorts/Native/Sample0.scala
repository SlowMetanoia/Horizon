package Shorts.Native

case class Sample0(){
  @native def intMethod(n: Int): Int
  @native def booleanMethod(b: Boolean): Boolean
  @native def stringMethod(s: String): String
  @native def intArrayMethod(a: Array[Int]): Int
}

object Sample0 {

  def main(args: Array[String]): Unit = {
    System.loadLibrary("Sample1")
    val sample = new Sample0
    val square = sample.intMethod(5)
    val bool = sample.booleanMethod(true)
    val text = sample.stringMethod("java")
    val sum = sample.intArrayMethod(Array(1, 1, 2, 3, 5, 8, 13))

    println(s"intMethod: $square")
    println(s"booleanMethod: $bool")
    println(s"stringMethod: $text")
    println(s"intArrayMethod: $sum")
  }
}