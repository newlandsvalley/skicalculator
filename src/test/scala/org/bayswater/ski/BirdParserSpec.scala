package org.bayswater.ski

import org.specs2.mutable._
import BirdParser._


class BirdParserSpec extends Specification {   
  
 "Normaliser" should {
    "normalise a Cardinal" in {
      val standardCardinal = "xzy"
      val cardinal = "(xz)y"
      normalise(standardCardinal) must_== cardinal      
    }
    "normalise a Cardinal twice removed" in {
      val standardCardinaltr = "xyzvw"
      val cardinaltr = "(((xy)z)v)w"
      normalise(standardCardinaltr) must_== cardinaltr    
    }
    "normalise an Eagle" in {
      val standardEagle = "xy(zwv)"
      val eagle = "(xy)((zw)v)"
      normalise(standardEagle) must_== eagle   
    }    
    "normalise a Dove" in {
      val standardDove = "xy(zw)"
      val dove = "(xy)(zw)"
      normalise(standardDove) must_== dove   
    }    
    "leave an already normalised standard Bluebird unaffected" in {
      val bluebird = "x(yz)"
      normalise(bluebird) must_== bluebird   
    }    
  }  
   
  "Bird parser" should {
    "recognize a Bluebird" in {  
      val bluebird = "x(yz)"
      roundTrip(bluebird) must_== bluebird
    }
    "recognize a normalised Cardinal" in {  
      val cardinal = "(xz)y"
      roundTrip(cardinal) must_== cardinal
    }
    "recognize a standard Cardinal" in {  
      val cardinal = "(xz)y"
      val standardCardinal = "xzy"
      roundTrip(normalise(standardCardinal)) must_== cardinal
    }
    "recognize a normalised Cardinal twice removed" in {  
      val cardinalr = "(((xy)z)v)w"
      roundTrip(cardinalr) must_== cardinalr
    }
    "recognize a standard Cardinal twice removed" in {  
      val cardinalr = "(((xy)z)v)w"
      val standardCardinalr = "xyzvw"
      roundTrip(normalise(standardCardinalr)) must_== cardinalr
    }
    "recognize a normalised Dove" in {  
      val dove = "(xy)(zw)"
      roundTrip(dove) must_== dove
    }
    "recognize a standard Dove" in {  
      val dove = "(xy)(zw)"
      val standardDove = "xy(zw)"
      roundTrip(normalise(standardDove)) must_== dove
    }
    "recognize a normalised Eagle" in {  
      val eagle = "(xy)((zw)v)"
      roundTrip(eagle) must_== eagle
    }
    "recognize a standard Eagle" in {  
      val eagle = "(xy)((zw)v)"
      val standardEagle = "xy(zwv)"
      roundTrip(normalise(standardEagle)) must_== eagle
    }
    "recognize a normalised Finch" in {  
      val finch = "(zy)x"
      roundTrip(finch) must_== finch
    }
    "recognize a normalised Hummingbird" in {  
      val hummingbird = "((xy)z)y"
      roundTrip(hummingbird) must_== hummingbird
    }
    "recognize a standard Hummingbird" in {  
      val hummingbird = "((xy)z)y"
      val standardHummingbird = "xyzy"
      roundTrip(normalise(standardHummingbird)) must_== hummingbird
    }
    "recognize an Identitybird" in {  
      val identitybird = "x"
      roundTrip(identitybird) must_== identitybird
    }
    "recognize a Starling" in {  
      val starling = "(xz)(yz)"
      roundTrip(starling) must_== starling
    }
  }
    
 
  
  def roundTrip(input: String): String = 
    flatten(parse(input))
  
 

}
