import org.scalatest._
import js.hw4.ast._
import HW4._

class HW4Spec extends FlatSpec {

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst { (i: Int) => if (i < 0) Some(-i) else None } (l1)
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  } 

  // Probably you want to write some tests for typeInfer, substitute, and step.
  "typeInfer" should "check the types are correct for inputs" in {
    val map: Map[String, Typ] = Map()
    val value = (typeInfer(map, Num(1)) == typeInfer(map, Num(2)))
    assert(value == true)
  }
  "substitute" should "properly substitute values into expressions" in {
    val x = Var("variable")
    val y = Num(1)
    substitute(x, "variable", y)
    assert(Num(1) == x)
  }
  "step" should "properly step through expressions" in {
    val x = Num(5)
    val y = BinOp(Plus, Num(4), Num(1))
    assert(step(y) == x)
  }
}
