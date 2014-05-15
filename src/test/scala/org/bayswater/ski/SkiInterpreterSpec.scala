package org.bayswater.ski

import org.specs2.mutable._
import BirdParser._
import AlphaEliminator._
import SkiInterpreter._

class SkiInterpreterSpec  extends Specification {
  "Ski Interpreter" should {
 
    "Interpret a Mockingbird" in {
      val mockingbird = "xx"
      roundTripInterpretation(mockingbird, "x") must_== mockingbird
    } 
    "Interpret a Thrush" in {  
      val thrush = "yx"
      val ski = "(S(K(SI)))K"
      roundTripInterpretation(thrush, "yx") must_== thrush
    }    
    "Interpret a Bluebird" in {  
      val bluebird = "x(yz)"      
      roundTripInterpretation(bluebird, "zyx") must_== bluebird
    }  
    "Interpret a Cardinal" in {  
      val cardinal = "(xy)z"      
      roundTripInterpretation(cardinal, "zyx") must_== cardinal
    }     
    "Interpret a Cardinal once removed" in {  
      val cardinal1 = "((xy)w)z"      
      roundTripInterpretation(cardinal1, "wzyx") must_== cardinal1
    }        
    "Interpret a Cardinal twice removed" in {  
      val cardinal2 = "(((xy)w)z)v"      
      roundTripInterpretation(cardinal2, "vwzyx") must_== cardinal2
    }    
    "Interpret a Dove" in {  
      val dove = "(xy)(zw)"      
      roundTripInterpretation(dove, "wzyx") must_== dove
    }     
    "Interpret an Eagle" in {  
      val eagle = "(xy)((zw)v)"      
      roundTripInterpretation(eagle, "vwzyx") must_== eagle
    }     
    "Interpret a Finch" in {  
      val finch = "(zy)x"      
      roundTripInterpretation(finch, "zyx") must_== finch
    }     
    "Interpret a Goldfinch" in {   
      val goldfinch = "(xw)(yz)"    
      roundTripInterpretation(goldfinch, "wzyx") must_== goldfinch
    }  
    "Interpret a Hummingbird" in {   
      val hummingbird = "xyzy"     
      val normalisedHummingbird = "((xy)z)y"    
      roundTripInterpretation(hummingbird, "zyx") must_== normalisedHummingbird
    }     
    "Interpret a Jay" in {   
      val jay = "(xy)((xw)z)"    
      roundTripInterpretation(jay, "wzyx") must_== jay
    }  
    "Interpret a Lark" in {  
      val lark = "x(yy)"    
      roundTripInterpretation(lark, "yx") must_== lark
    }  
    "Interpret a Queer bird" in {  
      val queerBird = "y(xz)"    
      roundTripInterpretation(queerBird, "zyx") must_== queerBird
    }  
    "Interpret a Quiotic bird" in {  
      val quioticBird = "x(zy)"    
      roundTripInterpretation(quioticBird, "zyx") must_== quioticBird
    } 
    "Interpret a Quirky bird" in {  
      val quirkyBird = "z(zy)"    
      roundTripInterpretation(quirkyBird, "zyx") must_== quirkyBird
    }   
    "Interpret a Robin" in {  
      val robin = "(yz)x"      
      roundTripInterpretation(robin, "zyx") must_== robin
    }   
    "Interpret a Turing bird" in {  
      val turingBird = "y((xx)y)"      
      roundTripInterpretation(turingBird, "yx") must_== turingBird
    }       
    "Interpret a Vireo" in {  
      val vireo = "(zx)y"      
      roundTripInterpretation(vireo, "zyx") must_== vireo
    }    
    "Interpret a Warbler" in {  
      val warbler = "xyy"      
      val normalisedWarbler = "(xy)y"
      roundTripInterpretation(warbler, "yx") must_== normalisedWarbler
    }   
    "Interpret a Warbler once removed" in {  
      val warbler1 = "xyzz"      
      val normalisedWarbler = "((xy)z)z"
      roundTripInterpretation(warbler1, "zyx") must_== normalisedWarbler
    }    
    "Interpret a Warbler twice removed" in {  
      val warbler2 = "xyzww"      
      val normalisedWarbler = "(((xy)z)w)w"
      roundTripInterpretation(warbler2, "wzyx") must_== normalisedWarbler
    } 
    "Interpret a Warbler intermediary" in {  
      val warbler = "(KI)x"    
      val skiTree = parse(normalise(warbler))  
      val interpretedTree = interpret(skiTree, "y")
      flatten(interpretedTree) must_== "y"
    }    
    "Interpret a Converse Warbler" in {  
      val converseWarbler = "xyy"      
      val normalisedWarbler = "(xy)y"
      roundTripInterpretation(converseWarbler, "yx") must_== normalisedWarbler
    }   
  }  
   
  def roundTripInterpretation(input: String, target: String): String = {
     val birdTree = parse(normalise(input))
     val skiTree = alphaEliminate(birdTree, target)
     val flattenedSki = flatten(skiTree)
     // println(s"source $input - ski tree $flattenedSki")
     val interpretedTree = interpret(skiTree, target)
     // println(s"unflattened interpreted tree $interpretedTree")
     flatten(interpretedTree)
   }
  
    
  
}