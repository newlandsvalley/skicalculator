package org.bayswater.ski

import SKINodes.{S,K,I}
import BirdParser.flatten

/** An Interpreter for trees of the SKI calculus, which, when applied to suitable variables,
 *  produces the corresponding combinator bird.  i.e. the reverse of the α-eliminator 
 * 
 */
object SkiInterpreter {

  /** Interpret a SKI tree by applying the variables and then walking the tree
   * 
   */
  def interpret(t: Tree, vars: String): Tree = {
    
    def toTree(xs: String): Tree = xs.length match {
      case 1 => Node(t,Leaf(xs.charAt(0).toString))
      case _ => Node(toTree(xs.drop(1)), Leaf(xs.charAt(0).toString))
    }
    
    val treeWithVariables = toTree(vars)
    
    // println(s"interpret: tree with variables $treeWithVariables")
    
    val result = interpret(treeWithVariables)
    // println(s"interpret result $result")
    result
  }
  
  /** interpret a branch of the tree, attempting to replace and S,K,I leaves
   *  with the target variables (as appropriate)
   */
  def interpret(t: Tree): Tree =  {
    
    // println(s"interpreting tree $t")
    
    t match {
  
      case leaf @ Leaf(x) => x match {
        case "S" => S0
        case "K" => K0
        case "I" => I0
        case x => leaf
      }
                      
      case node @ Node(l,r) => 
        if (fullyResolved(l))
          Node(l, interpret(r))
        else
          apply(interpret(l),r)           
      
      case x => scala.sys.error(s"unexpected node $x at top level")
      
    }
  }
  
  /** return true if the tree is fully resolved - i.e. only
   *  variables x,y,z....  remain
   */
  def fullyResolved(t: Tree): Boolean = t match {
    case Leaf(x) => x match {
      case "S" | "K" | "I" => false
      case _ => true
    }
    case Node(l, r) => fullyResolved(l) && fullyResolved(r)
    case _ => false
  }
    
  /** apply the left hand tree to the right hand tree
   * 
   */
  def apply(l: Tree, r: Tree): Tree = {
      
      // println(s"applying $l to $r")
      
      l match {
   
      
        case S0 => S1(r) 
        case K0 => K1(r)
        case I0 => r
        case I => r
        case K => K1(r)
        case S => S1(r)
      
        case K1(x) => {
          // println(s"K1 => $x, eliminating $r")
          x          
        }      
        case S1(x) => {
          val s = S2(x, r)
          // println(s"S1 => $s")
          s
        } 
        case S2(x,y) => { 
          // println(s"S2: x:$x, y:$y z:$r")
          val node = Node(Node(x,r), Node(y,r))
          // println(s"S2 => $node")
          interpret(node)
        } 
 
        case node @ Node(x,y) => 
          if (fullyResolved(l))
            Node(l, interpret(r))
          else
            interpret(Node(l, r))  
        
        case Leaf(x) => Node(l,interpret(r))
        
        case x => scala.sys.error(s"unexpected node $x in apply")
      }     
    }
}