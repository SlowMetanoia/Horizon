package Shorts.Graph.Test



case class Node[V](value:V) extends AnyVal
case class Edge[V,E](source:Node[V],receiver:Node[V],value:E)
case class Graph[V,E](nodes:Set[Node[V]],edges:Set[Edge[V,E]]){

}
object Graph{
  def from[V,E,CC <: Iterable[V]](elements:CC)(
    nodePred:V=>Boolean)(
    edgeRule:PartialFunction[(V,V),E]):Graph[V,E] = {
    def edgeFunc(nodes:(Node[V],Node[V])):PartialFunction[(V,V),Edge[V,E]] = {
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
  def from2[V,E,CC <: Iterable[V]](elements:CC)(edgePred:PartialFunction[(V,V),E]) = {
    Graph(
      nodes = elements.map(a => Node(a)).toSet,
      edges = elements.view.flatMap{i=>
        elements.collect{j=>
          edgePred.andThen(
            Edge(Node(i),Node(j),_)
          )(i,j)
        }
      }.toSet
    )
  }
}
