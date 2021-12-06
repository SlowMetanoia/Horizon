import scala.collection.mutable

/**
 * Классовый сахар и pattern matching
 */

/**
 * Кортежи в scala - тоже сладкие
 */
  //Хоба, кортежи.
val tuple0 = ("b",2)//всё, кортеж готов.

val (p1,p2) = tuple0
p1
p2
//Элементов в кортеже не может быть больше 22. Честно говоря, можно написать и свои кортежи на большее число значений
//но это крайне специфическая и часто вообще ненужная ерунда.

/**
 * Вот мы объявили класс, и сразу же объявили его главный конструктор.
 * @param name скажем, имя человека. public,const
 * @param age возраст. public
 * @param sex пол. public,const
 */
class Person(val name:String, var age:Int, val sex:Boolean){
  //это тело главного конструктора, выше - его параметры. Параметры главного конструктора становятся полями класса.
  println(this)
  //Дополнительный конструктор
  def this(age:Int,sex:Boolean){
    //Дополнительный конструктор первой операцией должнен вызывать конструктор, объявленный выше.
    this("Unknown",age,sex)
    println("I am additional!")
  }
  /**
   * Сахар №1 - apply()!
   */
  def apply(fieldNum:Int):String = {
    //просто switch?
    fieldNum match {
      case 1 => name
      case 2 => age.toString
      case 3 => if(sex) "male" else "female"
      //просто default?
      case _ => throw new MatchError("wrong field number")
    }
  }
  /**
   * Сахар №2 - update()!
   */
  def update(age:Int): Unit = this.age = age
  def update(p1:Int,p2:Int,age:Int): Unit = this.age = age
  override def toString = s"Person($name, $age, $sex)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Person]
  //на самом деле, тоже сахар...
  override def equals(other: Any): Boolean = other match {
    case that: Person =>
      (that canEqual this) &&
        name == that.name &&
        age == that.age &&
        sex == that.sex
    case _ => false
  }
  override def hashCode(): Int = (name,age,sex).##
}
//вызов главного конструктора
val person1 = new Person("Unknown",21,false)
//вызов дополнительного конструктора
val person2 = new Person(21,false)
//вызов person1.apply(1)
person1(1)
//вызов person1.apply(1)
person1(2)
//вызов person1.update(22)
person1() = 22
//вызов person1.update(1,1,23)
person1(1,1) = 23
person1
person2() = 23

//работает благодаря equals.
//по-умолчанию, obj1 == obj2 <=> ссылки на них равны. То есть по-умолчанию проверяется тождественность объектов
person1 == person2

/**
 * На массивах лучше видно удобство этого дела.
 */
val arr = new Array[String](3)
//arr.update(0,"zero")
arr(0) = "zero"
//arr.apply(0)
arr(0)
val arr1 = Array(3)


/**
 * Одноимённый с классом объект называется объект-компаньон. Отличие от обычного singleton только
 * в том, что имеет доступ к private-полям объектов класса, и объекты класса имеют доступ к private-полям компаньона.
 * В таком объекте удобно объявлять то, что в java имело бы приписку static
 */
object Person{
  /**
   * Сахар №3 - фабричный метод из компаньона!
   */
  def apply(name:String, age:Int, sex:Boolean) = new Person(name,age,sex)
  /**
   * пока неясный Сахар №4 - unapply
   * Вообще у такого рода методов есть своё название - экстракторы
   */
   def unapply(person: Person):Option[(String,Int,Boolean)] = Some((person.name,person.age,person.sex))
}

/**
 * match - не просто switch, хотя как switch, конечно, тоже работает, но его функционал куда шире.
 * 1. match умеет определять тип.
 * 2. match умеет использовать экстракторы и неявное присваивание для кортежей.
 */
(1,"str") match {
  case (a,b) => b + " "+ a //Если входящее значение - tuple2[Int,String], то Int упадёт в a,String - в b
  case _ => throw new MatchError("HOW???")
}

person1 match {
  //а вот это - использование экстрактора.
  case Person(a,b,c) => a+" "+b+" "+c
  //на самом деле, эта строчка - необязательна, просто правило хорошего тона.
  case _ => throw new MatchError("HOW???")
}

/**
 * Сахар №5 - case class
 * Такая запись создаёт класс с public-const полями,
 * автоматически генерирует методы equals и HashCode по этим полям,
 * автоматически генерирует toString,
 * добавляет фабричный метод в объект компаньон,
 * добавляет экстрактор в объект-компаньон,
 * что в совокупности очень сильно упрощает жизнь, избавляя от лишних строк кода
 * в остальном, такой класс ничем не отличается от обыкновенного класса.
 */
case class Inhuman(name:String,age:Int,sex:Boolean){
  println(this)
}
val v3:Any = Inhuman("Seth",4000,true)
v3 match {
  case inhuman: Inhuman => println(inhuman)
  case _ =>
}

v3 match {
  case Inhuman(name,age,sex) => println(s"My name is $name, I am ${if(sex)"male" else "female"} and $age y.o.")
}

val map0 = mutable.HashMap(1->"a",2->"b",3->"c")
val map1 = new mutable.HashMap[String,Int]
  for(el<-map0.keySet)  map1(map0(el)) = el
map0
map1

val map2 = for((k,v)<-map0) yield (v,k)















/**
 * доп.Сахар №1: Инфексная запись
 * Если case-class имеет 2 поля, то его экстрактор можно писать в инфексной форме:
 * (написано плохо, но честно говоря, я не знаю, как ещё иллюстрировать.)
 */
case class <->[A,B](p1:A, p2:B)
val v4 = <->(1,"abba")
v4 match {
  case a <-> b => println(a+" "+b)
}
