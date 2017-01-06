package week6_COLLECTIONS

import scala.collection.Map.WithDefault

/**
 *
 *	Map are partial FUNC: map(key) -> value which may throw exception
 *
 *	Map can be made total FUNC withDefaultValue
 *	(map get key) => Option , decompose with case class None and Some(x)
 *
 *
 *	How to add to Map
 *		map + (key -> value)
 *
 *	Factory method: gruopBy
 */

object MAP {
  val cap: Map[String, String] = Map("Vietnam" -> "Hanoi", "Finland" -> "Helsinki", "United State" -> "Washington DC")
                                                  //> cap  : Map[String,String] = Map(Vietnam -> Hanoi, Finland -> Helsinki, Unite
                                                  //| d State -> Washington DC)
  //	ok
  val capTotal = cap withDefaultValue "unknown country"
                                                  //> capTotal  : scala.collection.immutable.Map[String,String] = Map(Vietnam -> H
                                                  //| anoi, Finland -> Helsinki, United State -> Washington DC)
  
  capTotal("Vietnamm")                            //> res0: String = unknown country
  val c = capTotal + ("Canada" -> "Ottawa")       //> c  : scala.collection.immutable.Map[String,String] = Map(Vietnam -> Hanoi, F
                                                  //| inland -> Helsinki, United State -> Washington DC, Canada -> Ottawa)
  
  
  def getCap(country: String): String = (cap get country) match {
    case None => "Missing data"
    case Some(cap) => cap
  }                                               //> getCap: (country: String)String
  
  
  
  
  
  class Poly(terms0: Map[Int,Double]) {
  	def this(bindings: (Int, Double)*) = this(bindings.toMap)
  
  	val terms = terms0 withDefaultValue 0.0
  	
  	def + (other: Poly) =
  		new Poly((other.terms foldLeft terms)(addTerm))
  	
  	def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
  		val (exp, coeff) = term
  		terms + (exp -> (coeff + terms(exp)))
  	}
  	// old version
  	/*
  	def + (that: Poly) =  new Poly(terms ++ (that.terms map adjust))
  	
  	def adjust(term: (Int, Double)) : (Int, Double) = {
  		val (exp, coeff) = term // decompose
  		exp -> (coeff + terms(exp))
  	}
  	*/
  	override def toString() =
  		(for ( (exp, coeff) <- terms.toList.sorted ) yield coeff + "x^" + exp) mkString " + "
  }
  
  val pol1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> pol1  : week6_COLLECTIONS.MAP.Poly = 2.0x^1 + 4.0x^3 + 6.2x^5
	val pol2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
                                                  //> pol2  : week6_COLLECTIONS.MAP.Poly = 3.0x^0 + 7.0x^3
	
	pol1 + pol2                               //> res1: week6_COLLECTIONS.MAP.Poly = 3.0x^0 + 2.0x^1 + 11.0x^3 + 6.2x^5

}