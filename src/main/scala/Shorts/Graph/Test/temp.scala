package Shorts.Graph.Test

object temp extends App{
  def dividers(x:Int) = {
    var result = Seq.empty[Int]
    var c = 0
    while (c<math.sqrt(x)){
      if(x % c == 0) result = result.appended(x)
      c+=1
    }
  }

  println(dividers(2438756))
  val collection = 1 to 100
}
