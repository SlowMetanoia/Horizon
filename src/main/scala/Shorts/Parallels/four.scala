package Shorts.Parallels

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.math._

object four extends App{
  def calculateSeries(seriesFunc:(Double,Int)=>Double)(n:Int)(x:Double) =
    Future.reduceLeft {
      (1 to n).map{i => Future {
        seriesFunc(x, i) }}}(_+_)
  def calculateIntegral(function: Double=>Double)(distance:(Double,Double))(n:Int) = {
    val d = (distance._2 - distance._1)/n
    Future.reduceLeft {
      (1 to n).map {
        i => Future {
          function(i*d) * d }}}(_+_)
  }
  def factorial(n:Int) = (1 to n).product
  val futures = Seq(
  calculateSeries( (x,n)=> pow(x,n)/factorial(n)            )(10000)(1),
  calculateSeries( (x,n)=> n/pow(2,n)                       )(10000)(0),
  calculateSeries( (x,n)=> 1/pow(n+6,1.0/3)                 )(1000)(0),

  calculateIntegral(x=> 2*x - 3*x*x + 4*x*x*x + 5*pow(x,4)  )((-1,1))(100),
  calculateIntegral(x=> pow(12,sin(x))*cos(x)               )((0,Pi/2))(100),
  calculateIntegral(x=> 4* pow(x-2*x*x + 1.5*pow(x,3),1.0/3))((1,1000))(100)
  )
  val results = Future.sequence(futures)
  Await.result(results,10.seconds)
  println("results order: series a,b,c then integrals a,b,c")
  results.foreach(list=> println(list.mkString("\n")))
}
