import utils._

object Main extends App {
   val res1 =  noCircuit(List(1), List((1,3),(1,2),(2,3)))
   println(res1)
   
   val res2 = connectedGraph(List(1,2,3,4,5,6,7,8,9,10,11,12), 
       List((1,2),(1,5),(2,3),(2,4),(5,6),(6,7),(7,8),(7,10),(10,9), (10,11), (11,12))) // true
   println(res2)
   
   val res3 = isTree(List(1,2,3), List((1,2),(1,3)))
   println(res3)
}
