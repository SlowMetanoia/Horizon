package Educational.Sugar

import Shorts.FunctionalCalculationsDSL.FunctionCompositions._

import java.lang.Thread.sleep
import scala.language.implicitConversions

object demo extends App {
  val f1 = (a: Int) => a + 10
  val f2 = (a: Int) => a * 100
  val f3 = (a: Int) => a.toString
  val f4 = (s: String) => s.toCharArray
  val f5 = (cx: Array[Char]) => cx.map(c => (c + 4).toChar)
  val f6 = (cx: Array[Char]) => cx.mkString("")
  val f7 = f1 >>= f2 >>= f3 >>= f4 >>= f5 >>= f6 >>= println
  val f8 = (a:Int)=>{sleep(1000);println(f3(a))}
  println(f6(f5(f4(f3(f2(f1(23456789)))))))
  f7(23456789)
  val fk = f1<< {
    sleep(1000)
    f1 >>= f2 >>= f3 >> println
  }>>=f2<<{
    sleep(100)
    f1 >>= f2 >>= f3 >> println
  }>>println
  fk(20)
  case class MyInt(x:Int) {
    def ~~~~~(y:MyInt): MyInt = MyInt(this.x + y.x)
  }
  implicit def Int2MyInt(x:Int):MyInt = MyInt(x)
  val v2 = MyInt(1) ~~~~~ 1
  val v3 = 1 ~~~~~ 1


}
