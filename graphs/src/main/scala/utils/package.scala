import scala.util.control.Breaks._
import scala.collection.mutable._

package object utils {
  type Tuple_Int_Int = (Int, Int)
    
  /**
   * Collects distinct vertices from the list of edges. 
   */
  def vertices(edges: List[Tuple_Int_Int]): List[Int] = {
  	edges.flatMap(t => List(t._1, t._2)).distinct
  }
    
  /**
   * Collects vertices from the set of edges.
   */
  def vertices(edges: scala.collection.mutable.Set[Tuple_Int_Int]):Set[Int] = {
    edges.flatMap(t => Set(t._1, t._2))
  }
    
  /**
   * Adds a vertex to a mutable list of vertices.
   */
  def AddVertex(vertex: Int, vertices: ListBuffer[Int]): Unit = {
	  if (!vertices.exists(v => v == vertex )) {
		  vertices += vertex
		}

	  // println("Vertices: " + vertices)
  }
  
  /**
   * Adds a vertex to a mutable set of vertices.
   */
  def AddVertex(vertex: Int, vertices: Set[Int]): Unit = {
	  if (!vertices.exists(v => v == vertex )) {
		  vertices += vertex
		}
	  
	  // println("Vertices: " + vertices)
  }
  
  /**
   * Removes an edge from a set of edges
   */
  def RemoveEdge(edges: Set[Tuple_Int_Int], edge: Tuple_Int_Int): Unit = {
    edges -= edge
    
	  // println("Edges: " + edges)
  }
  
  /**
   * Removes an edge from a list of edges
   */
  def RemoveEdge(edges: ListBuffer[Tuple_Int_Int], edge: Tuple_Int_Int): Unit = {
    edges -= edge
    
	  // println("Edges: " + edges)
  }
  
  /**
   * Removes a vertex from a set of vertices
   */
  def RemoveVertex(vertices: Set[Int], vertex: Int): Unit = {
    vertices -= vertex
    
	  // println("Vertices: " + vertices)
  }
            
  /**
   * Visits @param edges_to_visit and selects only the edges whose vertices
   * match @param vertex. For each encountered edge, removes it 
   * from @param edges_to_visit, if the pair vertex is found in @param visited_vertices.
   * Then the other vertex is also added to the @param visited_vertices.
   * The loop ends when either the pair vertex is found in the @param visited_vertices
   * or there are no edges in @param edges_to_visit which @param vertex belongs to.  
   * 
   * @param vertex is the fix vertex used in iteration.
   * @param edges_to_visit has edges added based on discovery of unvisited edges 
   * 				or removed if the matching vertex was already visited. 
   * @param visited_vertices has vertices added when these are identified 
   * 				as pairs of @param vertex. 
   * @return is 
   * 				- "false" if the loop ends as a result of a pair vertex being 
   * 					found in @param visited_vertices.
   * 				- "true" if the loop ends gracefully.   
   */
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
        RemoveEdge(edges_to_visit, edge)
				//println("Edges to visit: " + edges_to_visit)
				
				AddVertex(the_other_vertex, visited_vertices)
      }
      else
        result = false
    }
    result
  }
  
  /**
   * Determines whether there are no circuits in the graph represented
   * by @param edges_to_visit, starting from the one-element list @param visited_vertices.
   * 
   * @param visited_vertices is a fix list consisting in one vertice to start with
   * @param edges_to_visit is a fix list of edges to visit
   * @return is
   * 				- "true" if there are no circuits in the @param edges_to_visit,
   * 					when starting traversal from @param visited_vertices   
   * 				- "false" if there are circuits
   */
  def noCircuit2(visited_vertices: List[Int], edges_to_visit: List[Tuple_Int_Int]): Boolean = {
    /**
     * Recursive version of a function that determines whether there are no circuits 
     * in the graph represented by @param edges_to_visit, starting from 
     * the one-element list @param visited_vertices. This iterates 
     * through @param edges_to_visit and invokes @function VisitEdgesAddVertices to update
     * the variables @var edges_to_visit_aux and @var visited_vertices_aux but also
     * figures out the existence of circuits starting from the current vertex. 
     * In case such circuit is identified, it breaks the iteration and the 
     * recursive call, returning false. Otherwise, it makes use of recursivity.
     * The current implementation will be further modified to fully take
     * advantage of tail recursion.
     * 
     * @param visited_vertices has vertices added by the @function VisitEdgesAddVertices
     * 				while circuits are not yet identified.
     * @param edges_to_visit has edges removed by the @function VisitEdgesAddVertices
     * 				while they are still not part of a circuit.
     * @return is
     * 			- "true" if there are no circuits in the @param edges_to_visit,
     * 					when starting traversal from @param visited_vertices   
     * 			- "false" if there are circuits
     */
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
 
  
  /**
   * Determines whether there are no circuits in the graph represented 
   * by @param edges.
   * 
   * @param edges is a fix list of edges to visit
   * @return is
   * 				- "true" if there are no circuits in the @param edges,
   * 				- "false" if there are circuits
   */
  def noCircuit(edges: List[Tuple_Int_Int]): Boolean = {
    /**
     * Non-recursive version of a function that determines whether there 
     * are no circuits in the graph represented by @param edges_to_visit, 
     * starting with an empty list @param visited_vertices. This iterates 
     * through @param edges_to_visit and uses the local variable @var reached_vertices
     * to determine whether a vertex of an edge in @param edges_to_visit
     * has been already visited. The @param edges_to_visit and @param visited_vertices
     * are also updated. The main loop is around @param vertices_to_visit and
     * it ends when either there are no vertices left or its iternal iteration that 
     * visits the edges containing the vertices in @param vertices_to_visit,
     * identifies a vertex which is already in @param reached_vertices.
     * The internal iteration adds vertices to @param vertices_to_visit 
     * and @param reached_vertices. It also removes the edges from @param edges_to_visit.
     * If internal iteration ends gracefully, the main loop adds vertixes
     * to @param visited_vertices and removes vertices from @vertices_to_visit.
     * 
     * @param visited_vertices has vertices added while circuits are not yet identified.
     * @param edges_to_visit has edges removed while they are still not part of a circuit.
     * @return is
     * 			- "true" if there are no circuits in the @param edges_to_visit,
     * 			- "false" if there are circuits
     */
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
            RemoveEdge(edges_to_visit, edge)
            
            // println("Edges to visit: " + edges_to_visit)
          }
        }
        
        AddVertex(vertex, visited_vertices)
        RemoveVertex(vertices_to_visit, vertex)

        //println("Vertices to visit: " + vertices_to_visit)           
      }
      true
  	}

  	// A graph without edges is considered to have no circuits. 
  	if (edges.length == 0)
  	  return true
  	
    var edges_to_visit = Set[Tuple_Int_Int](edges: _*)
    var graph_vertices_from_edges = vertices(edges_to_visit)
      
    var visited_vertices = Set[Int]()
    var vertices_to_visit = Set[Int](graph_vertices_from_edges.head)
    	
    noCircuitLoop(vertices_to_visit, visited_vertices, edges_to_visit)
  }
    
  /**
   * Determines whether a graph specified by vertices stored in @param vert
   * and edges stored in @param edges is connected.  
   *  
   * @param vert is the list of graph's vertices
   * @param edges is the list of graph's edges
   * @return is
   * 			 - "true" if the graph is connected
   * 			 - "false" otherwise
   */
  def connectedGraph(vert: List[Int], edges: List[Tuple_Int_Int]):Boolean = {
    /**
     * 
     * 
     * Non-recursive internal function that determines whether a graph 
     * specified by vertices stored is @param edges_to_visit is connected.  
     * The main is around @param vertices_to_visit. The internal iteration
     * adds the to @param vertices_to_visit vertices from @edges_to_visit
     * that belong to edges whose other ends match vertices in @vertices_to_visit.
     * It also removes such edges from the @param edges_to_visit.
     * Outside of the internal iteration. The main loop adds vertices
     * to @visited_vertices and removes vertices from @vertices_to_visit.
     * That is, the function attempts a maximum expansion the visited vertices.
     * 
     * @param vertices_to_visit is a mutable set that starts with 
     * 			the head of the graphs vertices set.
     * @param visited_vertices is a mutable set that starts as an empty set of vertices.
     * @param edges_to_visit is a mutable set that starts with the list of graph's edges.
     * @param original_vertices_size is a fix size of the set of edges collected from 
     * 			the set of graph's edges.
     * @return is
     * 		  - "true" is the size of the @param visited_vertices 
     * 				still matches the value of @param original_vertices_size
     * 				(all the vertices were visited successfully and the list
     * 				of @param visited_vertices is exhaustive).
     * 			- "false" otherwise.
     */
    def connectedGraphLoop(vertices_to_visit: Set[Int],
        visited_vertices: Set[Int], 
        edges_to_visit: Set[Tuple_Int_Int],
        original_vertices_size:Int):Boolean = {
      
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
              
              RemoveEdge(edges_to_visit, edge)

              //println("Edges to visit: " + edges_to_visit)
            }
          }
            
          AddVertex(vertex, visited_vertices)
          RemoveVertex(vertices_to_visit, vertex)

          //println("Vertices to visit: " + vertices_to_visit)           
      }
      
      visited_vertices.size == original_vertices_size
    }
    
    var edges_to_visit = Set[Tuple_Int_Int](edges: _*)
    var graph_vertices_from_edges = vertices(edges_to_visit)
    var graph_vertices = Set[Int](vert: _*)

    // The following are two initial validations for 
    // improper graph definitions.
    
    // A graph without vertices or with no vertices collected from edges
    // is considered to be disconnected.
    if (graph_vertices_from_edges.size == 0
        || graph_vertices.size == 0)
      return false
    
    // A graph not properly defined (the vertices do not match the ones
    // collected from edges), is considered to be disconnected.
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
  
  /**
   * Determines whether a graph defined by a list of vertices 
   * and a list of edges is a tree (i.e. it is connected and has circuits).
   * 
   * @param vert is an immutable list of graph's vertices
   * @param edges is an immutable list of graph's edges
   * 
   */
  def isTree(vert: List[Int], edges: List[Tuple_Int_Int]):Boolean = {
    //connectedGraph(vert, edges) && noCircuit2(List(vertices(edges)(0)), edges)
    connectedGraph(vert, edges) && noCircuit(edges)
  }
}