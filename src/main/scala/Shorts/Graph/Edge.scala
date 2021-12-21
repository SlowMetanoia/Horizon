package Shorts.Graph

abstract case class Edge[V,E](source:Node[V],receiver:Node[V],property:E)

