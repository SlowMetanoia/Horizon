package InfomationDefence

//Так. В роли битмапа у нас сегодня выступает Long
case class BitMap(var value:Long){

  import BitMap._

  //i-тый бит
  def apply(i:Int):Boolean = testBit(i)
  //присвоить i-тому биту значение
  //короче, bitMap(i) = true(1)/false(0)
  def update(i:Int,bitVal:Boolean): Unit = if(testBit(i) != bitVal) flipBit(i)
  def update(i:Int,bitVal:Int):Unit = if(testBit(i) == (bitVal==0)) flipBit(i)
  //значение i-того бита
  def testBit(i:Int):Boolean = (value & singleBitMask(i)) != 0
  //поднять i бит
  def setBit(i:Int):Unit = if(!testBit(i)) value+=singleBitMask(i)
  //опустить i бит
  def dropBit(i:Int):Unit = if(testBit(i)) value-=singleBitMask(i)
  //изменить значение бита на противоположное
  def flipBit(i:Int):Unit =
    if(testBit(i)) value -= singleBitMask(i)
    else value += singleBitMask(i)
    
  //Применить перестановку
  def transpose(transposition:Seq[Int]):Unit = {
    val bits = this.bits
    for(i<-transposition.indices) update(i,bits(transposition(i)))
  }
  //Создать новый битмап и применить перестановку к нему.
  def transposed(transposition:Seq[Int]):BitMap = {
    val bm = BitMap(this)
    bm.transpose(transposition)
    bm
  }
  //сформировать строку
  def toBinaryString(length:Int):String = {
    val bStr = value.toBinaryString
    if( bStr.length>length) throw new NumberIsTooBigException("number is too big or length is too small")
    ("0"*(length-bStr.length))++bStr
  }
  //поменять биты i и j местами
  def swapBits(i:Int,j:Int): Unit =
    if(testBit(i) ^ testBit(j)) {
      flipBit(i)
      flipBit(j)
    }
  //половинки числа, каждая убирается в Int
  def bitHalfs: (BitMap,BitMap) = (BitMap(bits.take(32)),BitMap(bits.drop(32)))

  //выбор битов по позициям. В точности то, что делает перестановка, но семантический смысл другой.
  def choose(bitPositions:Seq[Int]): BitMap =
    BitMap(for(i<-bitPositions) yield testBit(i))

  //сложение по модулю 2
  def ^ (other:BitMap):BitMap = BitMap(value ^ other.value)
  //длинна числа в битах
  def bitLength:Int = {
    var v = value
    var c = 0
    while (v!=0) {
      v = v >> 1
      c += 1
    }
    c
  }

  //непосредственно биты, обёрнутые в Boolean
  def bits: Seq[Boolean] = for(i<-0 until 64) yield testBit(i)
}

object BitMap{
  //ряды
  protected def series[T](prev:T)(next:T=>T):LazyList[T] = prev#::series(next(prev))(next)
  //бит-маски с единственной 1 в i-том бите
  val singleBitMask:Seq[Long] = series[Long](1)(_*2).take(64).force
  class NumberIsTooBigException(msg:String) extends Exception(msg)
  def apply(bits:Seq[Boolean]): BitMap = BitMap(
    bits
      .zip(singleBitMask)
      .collect{case (true,num) => num}.sum
  )
  //фабричные методы
  def apply(bitMap:BitMap): BitMap = BitMap(bitMap.value)
  def fromBitHalfs(bm0:BitMap,bm1:BitMap):BitMap = BitMap(bm0.bits.take(32).appendedAll(bm1.bits.take(32)))
}