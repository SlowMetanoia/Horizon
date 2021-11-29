/**
 * К коллекциям
 */

//Избыточность объявлений
var arr = Array(1,2,3,9,8,7)
arr(4) = 0
arr
arr = new Array(0)
arr


/**
 * Мутабельные коллекции.
 * В основном находятся в scala.collection.mutable
 * К их элементам можно обращаться на чтение и на запись.
 */
//Можно написать import и не мучиться с такими штуками, но это дело вкуса.
val mutColl = collection.mutable.HashMap[String,Int]()
//без двойных скобочек update работает криво(далее)
mutColl += (("Wew",10))
mutColl.addOne("Heh",7)
mutColl += ("Nan"->0)
/**
 * Эти 3 строчки не отличаются ничем. -> является синтаксическим сахаром, преобразующим элементы слева и справа в
 * Tuple2, то есть
 */
val v1 = "a"
val v2 = 52
val tuple1 = v1->v2
val tuple2 = (v1,v2)
/**
 * Опять же, эффект одинаков.
 */
