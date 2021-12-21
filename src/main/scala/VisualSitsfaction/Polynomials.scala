package VisualSitsfaction

object Polynomials {
  def makePolynomial(roots: IndexedSeq[ComplexNum]):IndexedSeq[ComplexNum] =
    roots.indices.appended(roots.length)
      .map{
        i=>
          math.pow(-1,i)*
            roots.combinations(i)
              .map(_.reduce(_ * _)).reduce(_ + _)
      }
}
