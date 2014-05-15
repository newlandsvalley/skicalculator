package org.bayswater.ski

import scala.util.parsing.combinator._

class BirdExpressions extends JavaTokenParsers {
   def expression: Parser[Tree] = term~term ^^ { case a~b => Node(a,b) } | term ^^ { x => x }
   def term: Parser[Tree] = terminal | bracket
   def bracket: Parser[Tree] = "("~expression~")" ^^ { case l~e~r => e } 
   def terminal: Parser[Tree] = ("S" | "K" | "I" | "x"| "y" | "z" | "w" | "v" ) ^^ { x => Leaf(x) } 
}

object BirdParser extends BirdExpressions {

  /**
   * Parse a normalised combinator bird expression
   */
  def parse(s: String): Tree = {
    parseAll(expression, s) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }  
  
  /**
   * Normalise a standard combinator bird.  For example:
   * 
   *   xyz => (xy)z
   *   xy(zy) => (xy)(zy)
   *   x(yz) => x(yz)       - i.e. unaffected
   */
  def normalise(text: String): String = {
    case class acc(phrase: String, accumulator: String)
    
    val ans = text.foldLeft(acc("","")) ((b, a) => a match {
      case '('  => acc("", b.accumulator + enbracket(b.phrase, true) + a)
      case ')' => acc("", b.accumulator + enbracket(b.phrase, false) + a)
      case x => acc(b.phrase + a, b.accumulator)
    })    
    ans.accumulator + enbracket(ans.phrase, false)
  }
  
  /**
   * Flatten a combinator bird parse tree to a String, representing branch nodes by brackets
   */
  def flatten(t: Tree): String = t match {
    case Leaf(x) => x
    // ignore brackets at the very top level where they're not necessary
    case Node(l, r) => flatten1(l) + flatten1(r)
    case x => scala.sys.error(s"undefined node in bird tree $x")
  }
  
  private def flatten1(t: Tree): String = t match {
    case Leaf(x) => x
    case Node(l, r) => ("(" + flatten1(l) + flatten1(r) +")")
    case x => scala.sys.error(s"undefined node in bird tree $x")
  }  
    
  /** Place brackets around a standard bird expression which consists only
   *  of variable symbols (i.e. no embedded brackets exist already) 
   * 
   */
  private def enbracket(xs: String, bracketLevel2: Boolean): String = {
    
    def toTree(xs: String): Tree = xs.length match {
      case 2 => Node(Leaf(xs.charAt(1).toString),Leaf(xs.charAt(0).toString))
      case _ => Node(toTree(xs.drop(1)), Leaf(xs.charAt(0).toString))
    }
    
    if (xs.length <= 1)
      xs
    else if (xs.length == 2) {
      if (bracketLevel2)
        "("+xs+")" 
      else
        xs
    }
    else {
      flatten(toTree(xs.reverse))
    }
  }
  
}


