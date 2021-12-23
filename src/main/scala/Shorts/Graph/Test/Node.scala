package Shorts.Graph.Test



case class Node[V](value:V)
case class Edge[V,E](source:Node[V],receiver:Node[V],value:E)
case class Graph[V,E](nodes:Set[Node[V]],edges:Set[Edge[V,E]]){

}
object Graph{
  def from[V,E](elements:_ <: Iterable[V])(
    nodePred:V=>Boolean)(
    edgeRule:PartialFunction[(V,V),E]):Graph[V,E] = {
    def edgeFunc(nodes:(Node[V],Node[V])):PartialFunction[(V,V),Edge[E,V]] = {
      edgeRule.andThen(edgeValue=>Edge(nodes._1,nodes._2,edgeValue))
    }
    val nodes = elements.view.filter(nodePred).map(elem => Node(elem)).toSet
    val edges = nodes.flatMap { i =>
      nodes.collect { j =>
        edgeFunc((i, j))(i.value,j.value)
      }
    }
    Graph(nodes,edges)
  }
}
