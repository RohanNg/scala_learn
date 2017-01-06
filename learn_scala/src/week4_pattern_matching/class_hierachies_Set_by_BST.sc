package week4_pattern_matching
// A set of integer value, using binary tree implementation
object s5 {

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(s: IntSet): IntSet
  }

  object Empty extends IntSet {
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def contains(x: Int) = false
    override def toString = " _ "
    def union(s: IntSet): IntSet = s
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def toString = "{ " + left + " " + elem + " " + right + " }"

    def union(s: IntSet): IntSet =
      ((left union right) union s) incl elem
  }

  val x = new NonEmpty(1, Empty, Empty).incl(3).incl(5)

  val y = new NonEmpty(2, Empty, Empty).incl(0).incl(4)

  x.union(y).toString
  
	val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
	val b: Array[IntSet] = a
}