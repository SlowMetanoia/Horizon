package Shorts.Simplex

import scala.annotation.tailrec
import scala.language.implicitConversions


class SimplexMethodCalculations private(completeTable:Array[Array[Rational]]){
  def zRow(table: Array[Array[Rational]]):Array[Rational] = table.map(arr=>arr.last).dropRight(1)
  def bCol(table: Array[Array[Rational]]):Array[Rational] = table.last.dropRight(1)
  def indexOfMin(array: Array[Rational]): Int = {
    var index = 0
    array.indices.foreach(i=>if(array(i)<array(index)) index = i)
    index
  }
  def indexOfMax(array: Array[Rational]):Int = {
    var index = 0
    array.indices.foreach(i=>if(array(i)>array(index)) index = i)
    index
  }
  var simplexTable: SimplexTable = SimplexTable(
    matrix = completeTable,
    basis = bCol(completeTable).indices.map(i=>"y"+i).toArray,
    free = zRow(completeTable).indices.map(i=>"x"+i).toArray
  )
  implicit def simplexTable2Array2D(simplexTable: SimplexTable):Array[Array[Rational]] = simplexTable.matrix
  def check(table: Array[Array[Rational]]): Boolean ={
    val zRow = this.zRow(table)
    val negative = zRow.indices.collect{
      case i:Int if zRow(i)<0 => i
    }
    if(negative.isEmpty)
      true
    else if (negative.forall{
      j=>{
        table.indices.exists{i=>
          table(i)(j)>0
        }
      }
    }) false
    else {
      println(table.map(_.mkString(",")).mkString("\n"))
      throw new Exception("Z=>+-Inf")
    }
  }
  @tailrec
  final def calculate:SimplexTable = {
    if(check(simplexTable))
      simplexTable
    else {
      val solvingCol = indexOfMin(zRow(simplexTable))
      val bCol = this.bCol(simplexTable)
      val solvingRow = indexOfMax(bCol.indices.toArray.collect{
        case i:Int if simplexTable(i)(solvingCol)>0 => i
      }.map { i=> bCol(i)/simplexTable(i)(solvingCol)
      }
      )
      simplexTable = simplexTable.simplexTransformation(solvingRow,solvingCol)
      calculate
    }
  }
}

object SimplexMethodCalculations{
  def apply(zRow: Array[Rational],
            bCol: Array[Rational],
            matrix:Array[Array[Rational]]):SimplexMethodCalculations = {
    val completeZRow = zRow.appended(Rational(0,1))
    new SimplexMethodCalculations(
      matrix.appended(bCol)
        .indices.toArray
        .map(i=>
          matrix(i).appended(completeZRow(i))))
  }
}