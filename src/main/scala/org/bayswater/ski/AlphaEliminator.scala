package org.bayswater.ski

import SKINodes.{S,K,I}
import scala.annotation.tailrec

object AlphaEliminator { 
  /** α-eliminate a set of variables in the target String from the tree by successively
   *  eliminating each from the returned tree of the previous elimination. This will replace
   *  all variables x,y,z etc. with S,K,I, suitably parenthesized.
   * 
   * i.e. typically target will be zyx or wzyx and so on
   */
  def alphaEliminate(t: Tree, target: String): Tree = {
    // println(s"α-eliminate step - $target from $t")
    if (1 == target.length)
      alphaEliminateOnce(t, target.charAt(0))
    else 
      alphaEliminate(alphaEliminateOnce(t, target.charAt(0)), target.drop(1))
  }
 
  /** α-eliminate a single variable from the tree
   * 
   */
  private def alphaEliminateOnce(t: Tree, target: Char): Tree = eliminate(t, target.toString) 
  
  /** α-eliminate implementation.  For an explanation of the algorithm see 
   *      
   *     to mock a Mockingbird
   *     Raymond Smullyan
   *     Chapter 18 - The Master Forest  
   */
  private def eliminate(t: Tree, target: String): Tree = {
    
    def result = t match {
      case leaf @ Leaf(x) => if (x == target)
                      principle1
                    else
                      principle2(leaf)
                      
      case node @ Node(l,r) => {
        
        val inLeft = contains(l, target)   
        val inRight  = contains(r, target)
        
        if (!inLeft && !inRight)
          principle2(node)
        else if (!inLeft) r match {
          case Leaf(x) =>  principle3(l)
          case _ => principle4(eliminate(l, target), eliminate(r, target))
        }
        else
          principle4(eliminate(l, target), eliminate(r, target))           
      }
    }
    // println(s"eliminate $target from $t result $result")
    result
  }
  
  /** Search this branch of the tree to see if the α-elimination target
   *  exists in the branch
   */
  private def contains(t: Tree, target: String): Boolean = t match {
    case Leaf(x) => x == target
    case Node(l,r) =>  contains(l, target) || contains(r, target)
  }
  
  /* fails to compile - match not exhaustive.  Nodes used by interpreter?
  private def contains(t: Tree, target: String): Boolean = {
    @tailrec def containsAcc(trees: List[Tree], acc: Boolean): Boolean = trees match {
      case Nil => acc
      case Leaf(x) :: rs => containsAcc(rs, (x == target) || acc)
      case Node(l, r) :: rs => containsAcc(l :: r :: rs, acc)
    }
    containsAcc(List(t), false)
  }
  * 
  */
  
  // α-elimination of α alone is I (Iα=α)
  private def principle1 = I
  // α-elimination of X (α not in X) is KX (KXα=X)
  private def principle2(t: Tree) = Node(K,t)
  // α-elimination of Yα (α not in Y) is Y (Yα=α)
  private def principle3(t: Tree) = t
  // α-elimination of XY (X an Y both α-eliminations) is SXY
  private def principle4(l: Tree, r: Tree) =  Node(Node(S,l),r)

}