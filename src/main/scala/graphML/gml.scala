package graphML

object gml extends App{
  
  import com.flowtick.graphs.Graph
  import com.flowtick.graphs.defaults._
  import com.flowtick.graphs.defaults.label._
  import com.flowtick.graphs.graphml._
  import com.flowtick.graphs.graphml.generic._
  
  val simple: Graph[Unit, String] =
    Graph.fromEdges(Set("A" --> "B", "B" --> "C", "D" --> "A"))
  
  val graphML = simple.asGraphML().xml
  //val loaded = FromGraphML[Int, String](graphML.toString)
}
