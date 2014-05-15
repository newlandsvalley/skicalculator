package org.bayswater.ski

import org.specs2.mutable._
import BirdParser._
import AlphaEliminator._

class AlphaEliminatorSpec extends Specification {
 
  "Alpha Eliminator" should {    

    "eliminate x from a Mockingbird" in {  
      val mockingbird = "xx"
      val ski = "(SI)I"
      eliminate1(mockingbird, "x") must_== ski
    } 
    "eliminate y from a Thrush" in {  
      val thrush = "yx"
      val ski = "(SI)(Kx)"
      flatten(eliminate(thrush, "y")) must_== ski
    }  
    "eliminate y and x from a thrush" in {  
      val thrush = "yx"
      val ski = "(S(K(SI)))K"
      flatten(eliminate(thrush, "yx")) must_== ski
    }
    "eliminate z from a Bluebird" in {  
      val bluebird = "x(yz)"
      val ski = "(S(Kx))y"
      flatten(eliminate(bluebird, "z")) must_== ski
    }
    "eliminate x from yx(xy)" in {  
      val bird = "yx(xy)"
      val ski = "(Sy)((SI)(Ky))"
      flatten(eliminate(bird, "x")) must_== ski
    } 
    "eliminate y from a warbler" in {  
      val warbler = "xyy"
      val ski = "(Sx)I"
      flatten(eliminate(warbler, "y")) must_== ski
    }
    "eliminate y and x from a warbler" in {  
      val warbler = "xyy"
      val ski = "(SS)(KI)"
      flatten(eliminate(warbler, "yx")) must_== ski
    }
    "eliminate w,z,y and x from a cardinal once removed" in {  
      val cardinal1 = "xywz"
      val ski = "(S((S(KS))((S(K(S(KS))))((S(K(S(KK))))(S(KS))))))(K(KK))"
      flatten(eliminate(cardinal1, "wzyx")) must_== ski
    }
    "eliminate v,w,z,y and x from an eagle" in {  
      val eagle = "xy(zwv)"
      val ski = "(S(K(S(KS))))((S(K(S(KK))))((S(K(S(KS))))(S(KK))))"
      flatten(eliminate(eagle, "vwzyx")) must_== ski
    }
  }
    
   def eliminate(input: String, target: String): Tree = {
     // println("normalised source " + normalise(input))
     val tree = parse(normalise(input))
     val t = alphaEliminate(tree, target)
     // println(s"unflattened eliminated tree $t")
     t
   }
   
   def eliminate1(input: String, target: String): String = {
     // println("α-elimination: normalised source " + normalise(input))
     val tree = parse(normalise(input))
     val skiTree = alphaEliminate(tree, target)
     val flat = flatten(skiTree)
     // println(s"α-elimination: result $flat")
     flat
   }
}