package week5_List

// CORE OPERATION OF LIST: head, tail, isEmpty
// Other operatiosn can be constrcuted from the core

// learn List through pattern matching
//		Nil 							=> Empty list
// 		p::ps 						=> head matching p, tail matching ps
//    List(p1,p2,..,pn) => same as p1::p2::p3::...::pn::Nil
//				List() 	== Nil		=> empty list
//				List(x)	== x::Nil => list with one element
//				1::2::xs          => list start with 1 and 2 // constant pattern matching

/**
 * List method
 *	observer: length, contains(x), indexOf(x)
 *	factory: head, tail, last, init, take, drop, ++, :::, reverse, ::, updated(x, n)
 */
object LearnList {

  // More, find the last element in a list
  val list = List(1, 5, 3, 4, 2)                  //> list  : List[Int] = List(1, 5, 3, 4, 2)



  /**
   * LAST
   */
  def last[T](l: List[T]): T = l match {
    case List() => throw new NoSuchElementException("Empty list")
    case List(x) => x
    case x :: xs => last(xs)
  }                                               //> last: [T](l: List[T])T

  last(list) == list.last                         //> res0: Boolean = true

  /**
   * INIT
   */
  def init[T](l: List[T]): List[T] = l match {
    // pattern amtching on list of size 0
    case List() => throw new Error("Empty list")
    // pattern matching on list of size 1
    case List(x) => List()
    // pattern matching on list of size >= 2 (size 1 was above)
    case x :: xs => x :: init(xs)
  }                                               //> init: [T](l: List[T])List[T]

  init(list).equals(list.init)                    //> res1: Boolean = true

  /**
   * CONCATERNATION
   * 	:::  associate to the right -> more efficient, type safe
   * 	++	 associate to the left, append any traversible
   */
  // O(|xs|)
  def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }                                               //> concat: [A](xs: List[A], ys: List[A])List[A]
  concat(List(1, 2), List("a"))                   //> res2: List[Any] = List(1, 2, a)
  concat(List(1, 2), List(3, 4)) == List(1, 2) ::: List(3, 4)
                                                  //> res3: Boolean = true
  List(1, 2) ++ List("a", "b", "c") == List(1, 2) ::: List("a", "b", "c")
                                                  //> res4: Boolean = true

  /**
   * REVERSE
   */
  // this have complexity O(n2)
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]
  reverse(list) == list.reverse                   //> res5: Boolean = true

  /**
   * 	REMOVEAT
   */
  def removeAt[T](n: Int, xs: List[T]): List[T] = {
    if (n == 0) xs match {
      case List() => xs
      case y :: ys => ys
    }
    else xs match {
      case List() => throw new IndexOutOfBoundsException
      case y :: ys => y :: removeAt(n - 1, ys)
    }
  }                                               //> removeAt: [T](n: Int, xs: List[T])List[T]
  def removeAtV2[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
                                                  //> removeAtV2: [T](n: Int, xs: List[T])List[T]
  removeAt(1, list) == removeAtV2(1, list)        //> res6: Boolean = true

  /**
   * 	TAKE
   */
  def take[T](n: Int, xs: List[T]): List[T] = {
    if (n == 0) List()
    else xs match {
      case List() => xs
      case y :: ys => y :: take(n - 1, ys)
    }
  }                                               //> take: [T](n: Int, xs: List[T])List[T]
  take(10, list) == (list take 10)                //> res7: Boolean = true
  take(4, list)                                   //> res8: List[Int] = List(1, 5, 3, 4)

  /**
   * DROP
   */
  def drop[T](n: Int, xs: List[T]): List[T] = {
    if (n == 0) xs
    else xs match {
      case List() => throw new IndexOutOfBoundsException("cannot drop")
      case y :: ys => drop(n - 1, ys)
    }
  }                                               //> drop: [T](n: Int, xs: List[T])List[T]
  drop(2, list) == (list drop 2)                  //> res9: Boolean = true

}