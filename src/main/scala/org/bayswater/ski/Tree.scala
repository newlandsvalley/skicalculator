package org.bayswater.ski

// parse tree 
trait Tree
// combinator bird nodes
case class Leaf(value: String) extends Tree
case class Node(left: Tree, right: Tree) extends Tree
// transient nodes used in SKI interpretation - essentially representations
// of partial application of S, K or I
case object S0 extends Tree
case object K0 extends Tree
case object I0 extends Tree
case class K1(child: Tree) extends Tree
case class S1(child: Tree) extends Tree
case class S2(left: Tree, right: Tree) extends Tree

object SKINodes {
  val S = Leaf("S")
  val K = Leaf("K")
  val I = Leaf("I") 
}
