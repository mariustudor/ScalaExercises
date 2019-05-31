package utils

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class GraphSuite extends FunSuite{
  import utils.noCircuit
  import utils.connectedGraph
  import utils.isTree
  
  test("noCircuitLoop: List(), List() === true") {
    assert(noCircuit(List(), List()) === true)
  }
  
  test("noCircuitLoop: List(1), List() === true") {
    assert(noCircuit(List(1), List()) === true)
  }
  
  test("noCircuitLoop: List(1), List((1,2))) === true") {
    assert(noCircuit(List(1), List((1,2))) === true)
  }
  
  test("noCircuitLoop: List(1), List((1,3),(1,2)) === true") {
    assert(noCircuit(List(1), List((1,3),(1,2))) === true)
  }
    
  test("noCircuitLoop: List(1), List((1,3)) === true") {
    assert(noCircuit(List(1), List((1,3))) === true)
  }
  
  test("noCircuitLoop: List(1), List((1,3),(1,2),(1,4),(2,5)) === true") {
    assert(noCircuit(List(1), List((1,3),(1,2),(1,4),(2,5))) === true)
  }
  
  test("noCircuitLoop: List(1), List((1,3),(1,2),(1,4),(4,5),(2,5)) === false") {
    assert(noCircuit(List(1), List((1,3),(1,2),(1,4),(4,5),(2,5))) === false)
  }
  
  test("noCircuitLoop: List(6), List((1,2),(1,5),(2,3),(3,4),(5,6),(6,7),(7,9),(6,8),(8,10),(10,11),(11,12)) === true") {
    assert(noCircuit(List(6), List((1,2),(1,5),(2,3),(3,4),(5,6),(6,7),(7,9),(6,8),(8,10),(10,11),(11,12))) === true)
  }
  
  test("connectedGraph: List(1), List() === false") {
    assert(connectedGraph(List(1), List()) === false)
  }
  
  test("connectedGraph: List(), List() === false") {
    assert(connectedGraph(List(), List()) === false)
  }
  
  test("connectedGraph: List(1,2), List() === false") {
    assert(connectedGraph(List(1,2), List()) === false)
  }
  
  test("connectedGraph: List(1,2), List((1,2),(1,3)) === false") {
    assert(connectedGraph(List(1,2), List((1,2),(1,3))) === false)
  }

  test("connectedGraph: List(1,2), List((1,2)) === true") {
    assert(connectedGraph(List(1,2), List((1,2))) === true)
  }

  test("connectedGraph: List(1,2,3,4), List((1,2),(1,3)) === false") {
    assert(connectedGraph(List(1,2,3,4), List((1,2),(1,3))) === false)
  }
  
  test("connectedGraph: List(1,2,3,4,5), List((1,2),(1,3),(4,5)) === false") {
    assert(connectedGraph(List(1,2,3,4,5), List((1,2),(1,3),(4,5))) === false)
  }

  test("connectedGraph: List(1,2,3,4,5,6,7,8,9,10,11,12), List((1,2),(1,5),(2,3),(2,4),(5,6),(6,7),(7,8),(7,10),(10,9), (10,11), (11,12)) === true") {
    assert(connectedGraph(List(1,2,3,4,5,6,7,8,9,10,11,12), List((1,2),(1,5),(2,3),(2,4),(5,6),(6,7),(7,8),(7,10),(10,9), (10,11), (11,12))) === true)
  }
  
  test("isTree: List(), List() === true") {
    assert(isTree(List(), List()) === false)
  }
  
  test("isTree: List(1), List() === true") {
    assert(isTree(List(1), List()) === false)
  }
  
  test("isTree: List(1), List((1,2))) === false") {
    assert(isTree(List(1), List((1,2))) === false)
  }
  
  test("isTree: List(1), List((1,3),(1,2)) === false") {
    assert(isTree(List(1), List((1,3),(1,2))) === false)
  }
  
  test("isTree: List(1,2), List() === false") {
    assert(isTree(List(1,2), List()) === false)
  }
  
  test("isTree: List(1,2), List((1,2),(1,3)) === false") {
    assert(isTree(List(1,2), List((1,2),(1,3))) === false)
  }
  
  test("isTree: List(1,2,3), List((1,2),(1,3)) === true") {
    assert(isTree(List(1,2,3), List((1,2),(1,3))) === true)
  }

  test("isTree: List(1,2), List((1,2)) === true") {
    assert(isTree(List(1,2), List((1,2))) === true)
  }

  test("isTree: List(1,2,3,4), List((1,2),(1,3)) === false") {
    assert(isTree(List(1,2,3,4), List((1,2),(1,3))) === false)
  }
  
  test("isTree: List(1,2,3,4,5), List((1,2),(1,3),(4,5)) === false") {
    assert(isTree(List(1,2,3,4,5), List((1,2),(1,3),(4,5))) === false)
  }

  test("isTree: List(1,2,3,4,5,6,7,8,9,10,11,12), List((1,2),(1,5),(2,3),(2,4),(5,6),(6,7),(7,8),(7,10),(10,9),(10,11),(11,12)) === true") {
    assert(isTree(List(1,2,3,4,5,6,7,8,9,10,11,12), 
        List((1,2),(1,5),(2,3),(2,4),(5,6),(6,7),(7,8),(7,10),(10,9),(10,11),(11,12))) === true)
  }
}