import scala.util.control.Breaks._
import scala.collection.mutable._
import java.util.Collection

package object utils {
  type Tuple_Int_Int = (Int, Int)
    
    def vertices(edges: List[Tuple_Int_Int]): List[Int] = {
 	  	edges.flatMap(t => List(t._1, t._2)).distinct
    }
    
    def vertices(edges: scala.collection.mutable.Set[Tuple_Int_Int]):Set[Int] = {
      edges.flatMap(t => Set(t._1, t._2))
    }
    
    def AddVertex(vertex: Int, visited_vertices: ListBuffer[Int]): Unit = {
  	  if (!visited_vertices.exists(v => v == vertex )) {
  		  visited_vertices += vertex
  		}
  		println("Visited vertices: " + visited_vertices)
	  }
    
    def AddVertex(vertex: Int, visited_vertices: Set[Int]): Unit = {
  	  if (!visited_vertices.exists(v => v == vertex )) {
  		  visited_vertices += vertex
  		}
  		println("Visited vertices: " + visited_vertices)
	  }
            
    def VisitEdgesAddVertices(vertex: Int,
           edges_to_visit: => ListBuffer[Tuple_Int_Int],
           visited_vertices: => ListBuffer[Int]): Boolean = {
      var edge:Tuple_Int_Int = (0,0)
      var result:Boolean = true
      println("Visited vertice: " + vertex)

      while (edges_to_visit.exists(e => e._1 == vertex || e._2 == vertex )
            && result == true) {
        edge = edges_to_visit.filter(e => e._1 == vertex || e._2 == vertex)(0)
        val the_other_vertex =  if (edge._1 == vertex) edge._2 else edge._1
        if (!visited_vertices.exists(v => v == the_other_vertex)) {
          edges_to_visit -= edge
					println("Edges to visit: " + edges_to_visit)
					
					AddVertex(the_other_vertex, visited_vertices)
        }
        else
          result = false
      }
      result
    }
   
    def noCircuit2(visited_vertices: List[Int], edges_to_visit: List[Tuple_Int_Int]): Boolean = {
      def noCircuitLoop(visited_vertices: ListBuffer[Int], 
          edges_to_visit: ListBuffer[Tuple_Int_Int]): Boolean = {
        
        var edges_to_visit_aux = edges_to_visit.clone()
        var visited_vertices_aux = visited_vertices.clone()
        var result = true
      
      	if (!edges_to_visit_aux.isEmpty) {
      	  breakable {
        		for (visited <- visited_vertices_aux) {
        		  result = VisitEdgesAddVertices(visited, edges_to_visit_aux, visited_vertices_aux)
              if (!result)
                break
      	    }
      	  }
      	  result && noCircuitLoop(visited_vertices_aux, edges_to_visit_aux)
       }
       else result
      }
      
      var edges_to_visit_aux = ListBuffer(edges_to_visit: _*)
      var visited_vertices_aux = ListBuffer(visited_vertices: _*)
      noCircuitLoop(visited_vertices_aux, edges_to_visit_aux)	
    }
   
    def noCircuit(edges: List[Tuple_Int_Int]): Boolean = {
    	def noCircuitLoop(vertices_to_visit: Set[Int],
          visited_vertices: Set[Int], 
          edges_to_visit: Set[Tuple_Int_Int]):Boolean = {
    	  
      	var vertex = 0
        var edges:Set[Tuple_Int_Int] = Set()
        var reached_vertices: Set[Int] = vertices_to_visit.clone()
        
        while(vertices_to_visit.size > 0) {
          vertex = vertices_to_visit.head
          
          if (edges_to_visit.exists(e => e._1 == vertex || e._2 == vertex)) {
            edges = edges_to_visit.filter(e => e._1 == vertex || e._2 == vertex)
            for (edge <- edges) {
              val the_other_vertex = if (edge._1 == vertex) edge._2 else edge._1
              
              if (reached_vertices.exists(v => v == the_other_vertex ))
                return false;
              
              AddVertex(the_other_vertex , vertices_to_visit)
              AddVertex(the_other_vertex, reached_vertices)
              edges_to_visit -= edge
              println("Edges to visit: " + edges_to_visit)
            }
          }
          
          AddVertex(vertex, visited_vertices)
          vertices_to_visit -= vertex
          println("Vertices to visit: " + vertices_to_visit)           
        }
        true
    	}
      
    	if (edges.length == 0)
    	  return true
    	
      var edges_to_visit = Set[Tuple_Int_Int](edges: _*)
      var graph_vertices_from_edges = vertices(edges_to_visit)
        
      var visited_vertices = Set[Int]()
      var vertices_to_visit = Set[Int](graph_vertices_from_edges.head)
      	
      noCircuitLoop(vertices_to_visit, visited_vertices, edges_to_visit)
    }
    
    def connectedGraph(vert: List[Int], edges: List[Tuple_Int_Int]):Boolean = {
      def connectedGraphLoop(vertices_to_visit: Set[Int],
          visited_vertices: Set[Int], 
          edges_to_visit: Set[Tuple_Int_Int],
          orignal_vertices_size:Int):Boolean = {
        
        var vertex = 0
        var result = true
        var edges:Set[Tuple_Int_Int] = scala.collection.mutable.Set() 
        
        while(vertices_to_visit.size > 0) {
            vertex = vertices_to_visit.head
            if (edges_to_visit.exists(e => e._1 == vertex || e._2 == vertex)) {
              edges = edges_to_visit.filter(e => e._1 == vertex || e._2 == vertex)
              for (edge <- edges) {
                val the_other_vertex = if (edge._1 == vertex) edge._2 else edge._1
                AddVertex(the_other_vertex , vertices_to_visit)
                edges_to_visit -= edge
                println("Edges to visit: " + edges_to_visit)
              }
            }
              
            AddVertex(vertex, visited_vertices)
            vertices_to_visit -= vertex
            println("Vertices to visit: " + vertices_to_visit)           
        }
        
        visited_vertices.size == orignal_vertices_size
      }
      
      var edges_to_visit = Set[Tuple_Int_Int](edges: _*)
      var graph_vertices_from_edges = vertices(edges_to_visit)
      var graph_vertices = Set[Int](vert: _*)

      if (graph_vertices_from_edges.size == 0
          || graph_vertices.size == 0)
        return false
      
      if ((graph_vertices_from_edges &~ graph_vertices).size > 0
          || (graph_vertices &~ graph_vertices_from_edges).size > 0)
        return false
        
      var visited_vertices = Set[Int]()
      var vertices_to_visit = Set[Int](graph_vertices.head)
      
      connectedGraphLoop(vertices_to_visit, 
          visited_vertices, 
          edges_to_visit, 
          graph_vertices_from_edges.size) 
	  }
    
    def isTree(vert: List[Int], edges: List[Tuple_Int_Int]):Boolean = {
      //connectedGraph(vert, edges) && noCircuit2(List(vertices(edges)(0)), edges)
      connectedGraph(vert, edges) && noCircuit(edges)
    }
}