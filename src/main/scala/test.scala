import scala.io.StdIn._
object test extends App{
  val a = readLine()
  val b = new Array[Char](a.length)
  def asmScheme = {
    val n = b.length - 1
    for (i<-a.indices)
      if (i <= n/2)
        b(i) = a(n-i)
      else
        b(i) = a(i-n/2-1)
  }
  asmScheme
  println(b.mkString(","))
}
