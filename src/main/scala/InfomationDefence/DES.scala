package InfomationDefence


//Кодирование при помощи DES
case class DES(key:String){
  import DES._
  //val key = "useless" //7 ascii-символов
  //последовательность ключей для шифрования DES по первым 7 символам инициализирующей ascii-строки
  def keys(someInput:String):Seq[BitMap] = {
    //первые 7
    val realKeyString = someInput.take(7)
    //проверки на корректность введённой строки
    if(realKeyString.length!=7)
      throw new IllegalArgumentException("Input string must contain at least 7 symbols")
    if(realKeyString.exists(_.toInt > 255))
      throw new IllegalArgumentException("Input string must contain only ascii symbols")
    val realKey = BitMap(
      realKeyString.zipWithIndex.map{case (char,i) => char.toInt * math.pow(256,i).toLong}.sum
    )
    //все возможные перестановки начального ключа
    val transpositions = {
      val transpositionArr = new Array[(Seq[Int],Seq[Int])](16)
      transpositionArr(0) = (keyC0,keyD0)
      for(i<- keyCyclingTable.indices.tail) transpositionArr(i) = {
        def cycleLeftShift[T](coll:Seq[T], shiftRange:Int): Seq[T] =
          series(coll){case Seq(h, rest @_*) => rest.appended(h)}(shiftRange)
        val last = transpositionArr(i - 1)
        (
          cycleLeftShift(last._1,keyCyclingTable(i)),
          cycleLeftShift(last._2,keyCyclingTable(i))
        )
      }
      transpositionArr.toSeq
    }.map(p=>p._1.appendedAll(p._2))

    val keys = transpositions.map(realKey.transposed).map(_.choose(keyChoose))
    keys
  }

  //ключи по 48 bit
  val allKeys: Seq[BitMap] = keys(key)

  //Функция Фейстеля
  def feistel(oldR:BitMap,key:BitMap): BitMap ={
    //копируем значение.
    var r = oldR
    //функция расширения
    r = r.choose(extensionTable.flatten)
    //сожжение с ключом по модулю 2
    r = r ^ key
    //функция сжатия
    val result = BitMap{
      r.bits.grouped(6).take(8).zip(compressionTables).map {
        case (bits, table) =>
          (Seq(bits.head, bits.last), bits.tail.init) match {
            case (i, j) => table(BitMap(i).value.toInt)(BitMap(j).value.toInt)
          }
      }.zipWithIndex.map {
        case (value, i) => value * math.pow(16, i).toLong
      }.sum
    }
    //перестановка
    result.transposed(feistelTransposition)
  }

  //кодирование 1 блока.
  def encodeBlock(x:Long):Long = {

    //смена представления
    val value = BitMap(x)
    //начальная перестановка
    value.transpose(startTransposition)
    //разделение на правую и левую части
    var (l,r) = value.bitHalfs

    //16-кратное преобразование сетью Фейстеля
    for(i<- 0 until 16){
      val (oldL,oldR) = (l,r)
      l = r
      r = oldL ^ feistel(oldR, allKeys(i))
    }

    //соединение левой и правой части, конечная перестановка.
    BitMap.fromBitHalfs(l,r).transposed(endTransposition).value
  }
  def decodeBlock(x:Long):Long = {
    //смена представления
    val value = BitMap(x)
    //начальная перестановка
    value.transpose(startTransposition)
    //разделение на правую и левую части
    var (l,r) = value.bitHalfs

    //16-кратное обратное преобразование сетью Фейстеля
    for(i<- (0 until 16).reverse){
      val (oldL,oldR) = (l,r)
      r = l
      l = oldR ^ feistel(oldL,allKeys(i))
    }

    //соединение левой и правой части, конечная перестановка.
    BitMap.fromBitHalfs(l,r).transposed(endTransposition).value
  }
  //кодирование в режиме электронной кодовой книги
  def ECBEncode[T](transformation:T=>Seq[Long],values:Seq[T]):Seq[Seq[Long]] =
    values
      .map(transformation)
      .map(_.map(encodeBlock))

  //декодирование в режиме электронной кодовой книги
  def ECBDecode[T](transformation:Seq[Long]=>T,encodedValues:Seq[Seq[Long]]):Seq[T] =
    encodedValues
      .map(_.map(decodeBlock))
      .map(transformation)

  //получение из ключа итератора по производным ключам
  def keyIterator(initVector:Long):Iterator[Long] =
    new Iterator[Long]{
      var currentKey: Long = initVector
      override def hasNext: Boolean = true
      override def next(): Long = {
        currentKey = encodeBlock(currentKey)
        currentKey
      }
    }

  //кодирование в режиме обратной связи по выходу
  def OFBEncode[T](transformation:T=>Seq[Long], initVector:Long, values:Seq[T]):Seq[Seq[Long]] = {

    //итератор по ключам.
    val keyStream: Iterator[Long] = keyIterator(initVector)

    //сложение по модулю 2 с ключами
    values
      .map(transformation)
      .map(_.map(_ ^ keyStream.next()))
  }

  //декодирование в режиме обратной связи по выходу
  def OFBDecode[T](transformation:Seq[Long]=>T,encodedValues:Seq[Seq[Long]],initVector:Long):Seq[T] = {
    //итератор по ключам.
    val keyStream: Iterator[Long] = keyIterator(initVector)

    //сложение по модулю 2 с ключами
    encodedValues
      .map(_.map(_ ^ keyStream.next()))
      .map(transformation)
  }
}

object DES{
  //всякие таблички для преобразований и перестановок
  //начальная перестановка
  val startTransposition = Seq(57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7, 56, 48, 40, 32, 24, 16, 8, 0, 58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14, 6)
  //конечная перестановка
  val endTransposition = Seq(39, 7, 47, 15, 55, 23, 63, 31, 38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29, 36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27, 34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9, 49, 17, 57, 25, 32, 0, 40, 8, 48, 16, 56, 24)
  //перестановка внутри функции Фейстеля
  val feistelTransposition = Seq(15, 6, 19, 20, 28, 11, 27, 16, 0, 14, 22, 25, 4, 17, 30, 9, 1, 7, 23, 13, 31, 26, 2, 8, 18, 12, 29, 5, 21, 10, 3, 24)
  //перестановки ключа
  val keyTransposition = Seq(56, 48, 40, 32, 24, 16, 8, 0, 57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18, 10, 2, 59, 51, 43, 35, 62, 54, 46, 38, 30, 22, 14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 60, 52, 44, 36, 28, 20, 12, 4, 27, 19, 11, 3)
  //частями
  val keyC0: Seq[Int] = keyTransposition.take(28)
  val keyD0: Seq[Int] = keyTransposition.drop(28)
  //табличка выбора битов ключа
  val keyChoose = Seq(13, 16, 10, 23, 0, 4, 2, 27, 14, 5, 20, 9, 22, 18, 11, 3, 25, 7, 15, 6, 26, 19, 12, 1, 40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47, 43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31)
  //табличка циклического сдвига
  val keyCyclingTable = Seq(1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1)
  //таблица расширения
  val extensionTable = Seq(
    Seq(31, 0, 1, 2, 3, 4),
    Seq(3, 4, 5, 6, 7, 8),
    Seq(7, 8, 9, 10, 11, 12),
    Seq(11, 12, 13, 14, 15, 16),
    Seq(15, 16, 17, 18, 19, 20),
    Seq(19, 20, 21, 22, 23, 24),
    Seq(23, 24, 25, 26, 27, 28),
    Seq(27, 28, 29, 30, 31, 0)
  )

  //таблицы сжатия
  val compressionTables = Seq(
    Seq(
      Seq(13, 3, 12, 0, 1, 14, 10, 7, 2, 9, 5, 11, 4, 8, -1, 6),
      Seq(-1, 14, 6, 3, 13, 1, 12, 0, 9, 5, 11, 10, 8, 4, 2, 7),
      Seq(3, 0, 13, 7, 12, 5, 1, 10, 14, 11, 8, 6, 2, 9, 4, -1),
      Seq(14, 11, 7, 1, 3, 8, 0, 6, 4, 10, 2, 13, 9, -1, 5, 12)
    ),
    Seq(
      Seq(14, 0, 7, 13, 5, 10, 2, 3, 8, 6, 1, 12, 11, -1, 4, 9),
      Seq(2, 12, 3, 6, 14, 1, 7, 13, 11, -1, 0, 9, 5, 8, 10, 4),
      Seq(-1, 13, 6, 10, 9, 3, 12, 0, 4, 7, 11, 5, 8, 2, 1, 14),
      Seq(12, 7, 9, 0, 2, 14, 3, 1, 10, 5, 6, 11, -1, 4, 13, 8)
    ),
    Seq(
      Seq(9, -1, 8, 13, 5, 2, 14, 4, 0, 12, 11, 6, 10, 3, 1, 7),
      Seq(12, 6, -1, 8, 2, 3, 5, 9, 1, 7, 4, 13, 11, 10, 14, 0),
      Seq(12, 5, 3, 8, 7, 14, 2, -1, 10, 0, 1, 11, 4, 9, 13, 6),
      Seq(0, 9, 12, -1, 5, 8, 7, 6, 3, 14, 13, 2, 10, 4, 1, 11)
    ),
    Seq(
      Seq(6, 12, 13, 2, -1, 5, 8, 9, 0, 1, 7, 4, 10, 11, 3, 14),
      Seq(12, 7, 10, 4, 5, 14, -1, 2, 3, 6, 1, 11, 0, 9, 13, 8),
      Seq(9, 5, 8, -1, 11, 10, 6, 12, 14, 0, 2, 13, 4, 1, 7, 3),
      Seq(2, 14, -1, 5, 9, 0, 12, 7, 8, 3, 4, 10, 11, 6, 1, 13)
    ),
    Seq(
      Seq(1, 11, 3, 0, 6, 9, 10, 5, 7, 4, 2, 14, 12, -1, 13, 8),
      Seq(13, 10, 1, 11, 3, 6, 12, 0, 4, -1, 14, 9, 2, 8, 7, 5),
      Seq(3, 1, 0, 10, 9, 12, 6, 7, 14, 8, 11, 4, 5, 2, -1, 13),
      Seq(10, 7, 11, 6, 0, 13, 1, 12, 5, 14, -1, 8, 9, 3, 4, 2)
    ),
    Seq(
      Seq(11, 0, 9, 14, 8, 1, 5, 7, -1, 12, 2, 3, 13, 6, 4, 10),
      Seq(9, 14, 3, 1, 6, 11, 8, 4, 5, 0, 12, 13, -1, 10, 2, 7),
      Seq(8, 13, 14, 4, 1, 7, 11, 2, 6, -1, 3, 9, 0, 12, 10, 5),
      Seq(3, 2, 1, 11, 8, 4, 14, 9, 10, 13, 0, 6, 5, -1, 7, 12)
    ),
    Seq(
      Seq(3, 10, 1, 13, 14, -1, 7, 12, 2, 11, 8, 6, 4, 9, 5, 0),
      Seq(12, -1, 10, 6, 3, 8, 0, 9, 13, 2, 4, 11, 1, 14, 7, 5),
      Seq(0, 3, 10, 12, 11, 2, 6, 13, 9, 14, 5, 7, -1, 4, 8, 1),
      Seq(5, 10, 12, 7, 0, 3, 9, 6, 8, 4, -1, 14, 13, 1, 2, 11)
    ),
    Seq(
      Seq(12, 1, 7, 3, 5, 14, 10, 0, 9, 8, 2, 13, 4, -1, 11, 6),
      Seq(0, 14, 12, 7, 9, 2, 6, 3, 11, 4, 5, 10, -1, 13, 8, 1),
      Seq(6, 10, 3, 0, 8, 11, 13, 1, -1, 5, 9, 12, 14, 2, 4, 7),
      Seq(1, 0, 13, 6, 3, 9, 7, 12, 14, 11, 8, -1, 2, 4, 5, 10)
    )
  )
}