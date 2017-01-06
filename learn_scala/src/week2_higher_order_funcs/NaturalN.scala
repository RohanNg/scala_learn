// Peano number
// this show how to encode primitive type as classes

package week2_higher_order_funcs

abstract class Nat {
  def isZero:        Boolean  
  def predecessor:   Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException("Zero have no prodecessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if(that.isZero) this else throw new NoSuchElementException("negative") 
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(predecessor + that)
  def - (that: Nat) = if(that.isZero) this else n - that.predecessor
}