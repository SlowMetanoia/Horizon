import scala.util.Random

val c = if(false) 1
(1 to 10).toList
def func(a:Int,b:Int):Int = a +b
def a:Int=>Int=>Int = a=>b=>a + b
def a:(Int,Int)=>Int = (a,b)=>a+b

val v = Set[Int]()
v.forall(elem=>elem==90)
val rand = Random

(1).formatted("%3d")

class A
class B extends A
case class Pair[T](p1:T,p2:T)

val c = Pair(new A,new A)
class B(x:Double){
  def +(y:Double):B = {
    new B(x + y)
  }
  override def toString = s"B($x)"
}
val b = new B(3)
val b1 = b + 1 + 2 + 4
b1 + 4
b