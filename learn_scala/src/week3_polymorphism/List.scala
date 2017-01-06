package week3_polymorphism

// trait is like abstract class, without construction parameter
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("nil.head")
  def tail: Nothing = throw new NoSuchElementException("nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, Nil)
  def apply[T]() = Nil
}

// call method on List object is interesting
// we can call List(), List(1,2), List(1)