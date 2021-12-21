package Shorts.Graph

abstract case class Graph[V,E](nodes:Set[Node[V]]) extends Iterable[V]{
  def uniteGraph(other:Graph[V,E]):Graph[V,E]
  def iterator: Iterator[V]

}