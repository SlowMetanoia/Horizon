package InfomationDefence

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

object RSA extends App{
  //Экземпляр ГПСЧ
  val rand = new scala.util.Random()
  //Ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //Не совсем натуральные числа
  val N = series[Int](0)(_ + 1)
  //Решето Эратосфена
  def erotothfenFilter: Int => Seq[Int] = n=> {
    val naturals = N.take(n).toArray
    for{
      i<-naturals
      if i>1
    } {
      var x = 2*i
      while (x<naturals.length) {
        naturals(x) = 0
        x = x + i
      }
    }
    naturals.filter(_!=0)
  }
  //Первые сколько-то простых
  val somePrimeNumbers = erotothfenFilter(10000).drop(1)
  //Генерирование больших простых чисел с применением критерия полингтона
  def generateBigPrime(edge:BigInt):BigInt  = {

    // выбираем случайное простое число
    val initialPrime = somePrimeNumbers(rand.nextInt(somePrimeNumbers.length))

    //Получение большого случайного числа в границах от l до r
    def bigRandomInt(l:BigInt,r:BigInt)={
      val cut = r - l
      val cutBytes = cut.toByteArray
      val resultBytes = rand.nextBytes(cutBytes.length - 1)
      val cutLeadingByte = cutBytes.head
      val resultLeadingByte = cutLeadingByte.sign match {
        case 0 => 0.byteValue
        case 1 => rand.nextInt(cutLeadingByte).byteValue
        case -1 => rand.nextInt(255 + cutLeadingByte).byteValue
      }
      BigInt(resultBytes.prepended(resultLeadingByte))
    }

    //медленное возведение в степень
    def pow(x: BigInt, y: BigInt): BigInt = {
      var result:BigInt = 1
      var c = y
      while(c>0) {
        result *= x
        c-=1
      }
      result
    }

    //увеличение числа с проверкой методом Поллингтона
    @tailrec
    def primeIncreasing(s: BigInt, edge: BigInt): BigInt = s match {
      //Если s > нужной нам границы - возвращаем s
      case s if s > edge => s
      case s =>
        //выбираем случайное чётное от s до 2(2s+1)
        var rnd = bigRandomInt(s, 2 * (2 * s + 1))
        while (rnd % 2 != 0) rnd = bigRandomInt(s, 2 * (2 * s + 1))
        //вычисляем число-кандидат на простоту
        val candidate = s * rnd + 1
        //если кандидат делится на простое и сам не является известным простым - начинаем сначала
        if (somePrimeNumbers.exists(candidate % _ == 0) && !somePrimeNumbers.contains(candidate))
          primeIncreasing(s, edge)
        else {
          //берём случайное от 1 до кандидата
          val a = bigRandomInt(1, candidate)
          //println(s"candidate = ${ candidate }")
          //println(s"a = ${ a }")
          //проверяем критерием Поллингтона, если всё ok
          if ((a.modPow(candidate - 1, candidate) == 1) && ((pow(a, rnd) - 1).gcd(candidate) == 1)) {
            primeIncreasing(candidate, edge)
          } else primeIncreasing(s, edge)
        }
    }
    primeIncreasing(initialPrime,edge)
  }
  //расширенный алгоритм Евклида для нахождения обратного по модулю
  def inverse(a:BigInt,n:BigInt) = {
    var t:BigInt = 0
    var r:BigInt = n
    var newT:BigInt = 1
    var newR:BigInt = a
    var ot:BigInt = 0
    var or:BigInt = 0
    var qoutient:BigInt = 0
    while (newR!=0) {
      qoutient = r / newR
      ot = t
      or = r
      t = newT
      newT = ot-qoutient*newT
      r = newR
      newR = or-qoutient*newR
    }
    t+n
  }

  //Объекты ключей
  abstract class Key(k0:BigInt,k1:BigInt)
  case class PrivateKey(d:BigInt,n:BigInt) extends Key(d,n)
  case class PublicKey(e:BigInt,n:BigInt) extends Key(e,n)
  val random = new Random()
  //Генерация ключей
  def newKeyPair:(PublicKey,PrivateKey) = {
    //Генерим большие простые по 1024 бита
    val (q,p) = (BigInt.probablePrime(1024,random),BigInt.probablePrime(1024,random))
    //модуль
    val n = p*q
    //функция Эйлера от наших простых
    val fi = (p-1)*(q-1)
    //открытая экспонента
    val e:BigInt = 65537
    //мультипликативно-сопряженное с e по модулю fi, закрытая экспонента
    val d = e.modInverse(fi)
    //inverse(e,fi)
    (PublicKey(e,n),PrivateKey(d,n))
  }
  //char => 2 byte
  def char2Bytes(ch:Char):Seq[Byte] = Seq((ch/256).toByte,(ch%256).toByte)
  //2 byte => char
  def bytes2Char(bytes:Array[Byte]):Char = (bytes.head * 256 + bytes.last).toChar
  //шифрование блока
  def encodeBlock(str:String,publicKey: PublicKey):BigInt = {
    val bytes = str.flatMap(char2Bytes).toArray
    BigInt( bytes ).modPow(publicKey.e,publicKey.n)
  }
  //расшифровывание блока
  def decodeBlock(bi:BigInt,privateKey: PrivateKey):String = {
    var bytes = bi.modPow(privateKey.d,privateKey.n).toByteArray
    if(bytes.length%2==1) bytes = bytes.prepended(0)
    bytes.grouped(2).map(bytes2Char).mkString
  }
  //шифрование текста
  def encode(str: String, key: PublicKey):Seq[BigInt] = {
    str.grouped(10).toSeq.map(encodeBlock(_,key))
  }
  //расшифровывание текста
  def decode(values: Seq[BigInt], key: PrivateKey):String = {
    values.map(decodeBlock(_,key)).mkString
  }

  //генерируем пару ключей
  val (publicKey,privateKey) = newKeyPair

  //Ввод-вывод для примеров.
  println("Input source text:")
  val sourceText = StdIn.readLine()

  println("encoded text:")
  val encoded = encode(sourceText,publicKey)
  println(encoded)

  println("decoded text:")
  val decoded = decode(encoded,privateKey)
  println(decoded)

}
