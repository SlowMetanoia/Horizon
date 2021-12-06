package SADMOR.KomiVoyager

import SADMOR.KomiVoyager
import SADMOR.KomiVoyager._

object test extends App {
  val arr = Array(
    Array(9000, 10, 20, 5, 10, 8),
    Array(10, 9000, 15, 20, 5, 0),
    Array(15, 20, 9000, 10, 30, 20),
    Array(10, 15, 10, 9000, 20, 5),
    Array(15, 25, 20, 14, 9000, 10),
    Array(10, 5, 20, 10, 8, 9000)
  )
  /*def Transpose(array: Array[Array[Int]]):Array[Array[Int]] = {
    for(i<-array.indices) yield
      for
  }*/
  val voyages = (for(i<- 0 to 5) yield KomiVoyager.Voyage(arr,i)).toMap
  val min = voyages.keySet.min
  println(s"minimum price is $min done by way ${voyages(min).reverse.mkString("->")}")
}