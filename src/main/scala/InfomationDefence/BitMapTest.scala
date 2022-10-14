package InfomationDefence

object BitMapTest extends App{
  val bm = BitMap(144)
  println(s"bm = $bm")
  bm.transpose(Seq(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
  println(s"bm = $bm")
  println(s"bm.bits = ${bm.bits}")
  println(s"BitMap(bm.bits) = ${BitMap(bm.bits)}")
  println(s"BitMap.fromBitHalfs(bm,bm).bits = ${BitMap.fromBitHalfs(bm,bm).bits}")
}
