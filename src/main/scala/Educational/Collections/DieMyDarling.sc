import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * Блеск монадичности и сложность кода.
 */


/**
 * <определения>
 * одноместная функция - A=>B
 * предикат - функция A=>Boolean, где A - некоторой произвольный тип.
 * двуместная функция - (A,B)=>C
 *
 * partition - частично определённая функция - A=>B, но если функция не определена на аргументе - кинет MatchError
 * </определения>
 */

//PlaceHolder
def func0:Int=>String = _.toString//переданный инт подставится вместо _
func0(3456)
//Это не кортеж и не карринг, это просто 2 значения, то есть просто двуместная функция
def func1:(Int,String)=>Boolean = _.toString == _
func1(10,"10")
//предикат
def func2:Int=>Boolean = _%2==1
//частоично определённая функция
def func3:Int=>Int = {
  case 1 => 1
  case 2 => 2
  case 3 => 6
}
//дальше
def func3AndAHalf:PartialFunction[Int,Int]= {
  case 1 => 1
  case 2 => 2
  case 3 => 6
}
func3(1)
func3(3)
//func3(10) - MatchError

//При передаче нескольких параметров в ЧОФ, формируется кортеж, при этом сами параметры передаются как обычно.
def func4:(Int,Int)=>Int = {
  case (1,2) => 1
}
func4(1,2)
//func4(5,3) - MatchError

val coll0 = IndexedSeq(1,2,3,4,5)
//map
val coll1 = coll0.map(el=>el*2)//map(A=>B)
val coll2 = coll0.map(_*2)
val coll3 = coll0.map(_.toString)
//filter
val coll4 = coll0.filter(el=>el%2==0)
val coll5 = coll0.filter(_%2==1)
val coll6 = coll0.filter(func2)
//collect = map + filter или около того
val coll7 = coll0.collect {
  case 1 => 1
  case 2 => 2
  case 3 => 6
}
val coll8 = coll0.collect{func3AndAHalf}

val collection0 = IndexedSeq[Any](1,"2fds","234",1.234,"-10.3")

val collection1 = collection0.collect {
  case d:Double                               => d
  case i:Int                                  => i.toDouble
  case s:String if s.toDoubleOption.isDefined => s.toDouble
}


//exists = true, если существует элемент, удовлетворяющий предикату.
collection1.exists(x=>x*2 == 2)

//contains = true, если включает в себя такой элемент
collection1.contains(1)

//forall = true, если предикат = true для каждого
collection1.forall(_>0)

//foreach - как map, но не возвращает значений
collection1.foreach(println)

/**
 * Свои коллекции:
 */
case class MyArray[A:ClassTag](array: Array[A])extends Iterable[A]{
  override def iterator:Iterator[A] = new Iterator[A]{
    var index = 0
    override def hasNext: Boolean = index<array.length
    override def next(): A = {index+=1;array(index-1)}
  }
}

var arr = MyArray(Array(1,2,3,4))
arr.map(elem=>elem*2)





















/**
 * Борьба за чистоту.
 */
val seq0 = 1 to 100

var seq1 = Seq.empty[Int]


/**
 * Результат тут не меняется, но из-за того, что предикат "грязный", изменится сайд-эффект.
 * Отсюда:
 * Всё, что можно сделать чистым, нужно делать чистым.
 * Если что-то нельзя сделать чистым, это нужно изолировать от остального кода.
 */
val val0 = seq0.filter{ x=> seq1 = seq1.appended(x);x>50 }.headOption
val val1 = seq0.find{ x=> seq1 = seq1.appended(x);x>50}


/**
 * Итак, зоопарк коллекций в scala:
 */
Map         // словарь/ассоциативный массив - массив, где индексы и значения - произвольного типа.
Seq         // последовательность - упорядоченная последовательность элементов.
IndexedSeq  // индексированная последовательность - местный аналог вектора (хотя класс Vector - тоже имеется)
List        // список - он и в африке список. Упорядоченный, с некоторыми дополнениями.
Array       // ну, всем нам знакомый массив.
ArrayBuffer // как Array, но с возможностью изменения длинны

/**
 * Есть и другие, более специфические вещи, но мне не кажется, что стоит разбирать их здесь
 */

/**
 * Почти у всех коллекций есть мутабельный и иммутабельный вариант.
 */

//----------------------------------------------------Tips and Tricks---------------------------------------------------
/**
 * Общий посыл тут такой: если вы знаете способ написать очевидно - пишите очевидно.
 * Если можно заменить самописную лямбду в методе вызовом существующего метода - сделайте это.
 * Используйте как можно меньше отрицаний в коде, как правило их можно заменить.
 * Далее по 2 строчки: 1 как не надо, 2 как надо
 */

  //empty для создания пустых коллекций явно.
val seq2 = Seq[Int]()
val seq3 = Seq.empty[Int]

seq2.filter(_==10).headOption
seq2.find(_==10)

//выход один и тот-же, но length в некоторых случаях быстрее
seq2.size
seq2.length

seq2.length == 0
seq2.isEmpty

!seq2.isEmpty
seq2.nonEmpty

seq1(0)
seq1.head

seq1(seq1.length-1)
seq1.last

if (seq1.nonEmpty) Some(seq1.head) else None
seq1.headOption

seq1.lift(0)
seq1.headOption

Range(0, seq1.length)
seq1.indices

seq1.zip(seq1.indices)
seq1.zipWithIndex

// несмотря на то, что они похожи, если в коллекции существует скрытое индексирование, второе будет быстрее +
// второе выразительнее
seq1.exists(_ == 1)
seq1.contains(1)

seq1.count(x=>x*2==2) > 0
seq1.exists(x=>x*2==2)

seq1.filter(x=>x*2==2).isEmpty
seq1.exists(x=>x*2==2)

//  и т.д.
//  https://habr.com/ru/post/333362/

