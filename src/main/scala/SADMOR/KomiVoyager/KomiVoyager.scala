package SADMOR

import scala.annotation.tailrec
import scala.collection.mutable

package object KomiVoyager{
  def Voyage(weights: Array[Array[Int]]): (Int, List[Int]) = {
    case class Voyager(path:List[Int],price:Int) {
      def go(way: Int): Voyager = Voyager(way :: path, price + weights(path.head)(way))
      override def toString: String = s"path = $path\n" +
        s"price = $price"
    }
    /**
     * Преобразование вояжеров в Ordering. Нужно для очереди с приоритетом. Приоритет больше у того, у кого меньше price.
     */
    implicit object Voyager2Ordering extends Ordering[Voyager]{
      override def compare(x: Voyager, y: Voyager): Int = y.price - x.price
    }
    @tailrec
    def findTheBestWay(voyagers:mutable.PriorityQueue[Voyager]):Voyager = {
      val voyager = voyagers.dequeue
      val newVoyagers = weights.indices.filterNot(voyager.path.contains(_))
        .map(voyager.go)
      if(newVoyagers.isEmpty)
        voyager
      else
        findTheBestWay(voyagers ++ newVoyagers)
    }
    val v = findTheBestWay(mutable.PriorityQueue().addAll{
      weights.indices.map(i=>Voyager(List(i),0))
    }
    )
    v.price->v.path
  }
}