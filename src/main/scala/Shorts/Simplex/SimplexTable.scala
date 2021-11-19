package Shorts.Simplex

import scala.language.implicitConversions


case class SimplexTable(matrix: Array[Array[Rational]],
                        basis:  Array[String],
                        free:   Array[String]) {
  def simplexTransformation(a: Int, b: Int): SimplexTable = {
    val X = a - 1
    val Y = b - 1
    val SolveElem = matrix(X)(Y)

    def replace(arr:Array[String],index:Int,value:String):Array[String] =
      arr.take(index).appended(value) ++ arr.drop(index + 1)

    SimplexTable(
      basis = replace(basis,X,free(Y)),
      free = replace(free,Y,basis(X)),
      matrix = for (i <- matrix.indices.toArray) yield
        for (j <- matrix(i).indices.toArray) yield {(i, j) match {
          case (X, Y) => Rational(1, 1) / SolveElem
          case (X, _) => matrix(i)(j) / SolveElem
          case (_, Y) => -matrix(i)(j) / SolveElem
          case (_, _) => (matrix(i)(j) * SolveElem - matrix(X)(j) * matrix(i)(Y)) / SolveElem
        }}.simplify()
    )
  }

  override def toString: String =
    s"Basis = (${basis.mkString(",")})\n" +
      s"Free  = (${free.mkString(",")})\n" +
      s"\n" +
      s"${matrix.map(strs=>strs.mkString(" ")).mkString("\n")}"
}