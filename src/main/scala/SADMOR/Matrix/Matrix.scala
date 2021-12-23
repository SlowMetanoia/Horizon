package SADMOR.Matrix

case class Matrix(mtx: IndexedSeq[IndexedSeq[Double]]){
  def transpose(mtx:IndexedSeq[IndexedSeq[Double]]): IndexedSeq[IndexedSeq[Double]] =
    for(i<-mtx.indices) yield
      for(j<-mtx(i).indices) yield mtx (i)(j)

  def transposed: Matrix = Matrix(transpose(mtx))
  def rows: IndexedSeq[IndexedSeq[Double]] = mtx
  def cols: IndexedSeq[IndexedSeq[Double]] = transpose(mtx)
  def withoutRow(i:Int): Matrix = Matrix(mtx.take(i) ++ mtx.drop(i+1))
  def withoutCol(i:Int): Matrix = transposed.withoutRow(i).transposed
  override def toString: String = mtx.map(row=>row.mkString(" ")).mkString("\n")
}
