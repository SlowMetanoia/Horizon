


(1 to 10).map{
  case a:Int if a % 2 == 0 => Some(a)
  case _:Int => None
}
































val a = Array[Char]('a','b','c','f','n','h','l')
val b = new Array[Char](a.length)
def asmScheme( ): Unit = {
  val n = b.length - 1
  for (i<-a.indices)
    if (i <= n/2)
      b(i) = a(n-i)
    else
      b(i) = a(i-n/2-1)
}
asmScheme
b