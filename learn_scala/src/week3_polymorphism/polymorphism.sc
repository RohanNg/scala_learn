package week3_polymorphism
import week3_polymorphism.List._
/**
 * Polymorphism: " in many forms "
 * Two principal form:
 * + sub typing (inheritances, traits): instance of sub type class can be passed to base class
 * + generics: instance of a function or class are created by type parameterization
 */
 
 
 
// Immutable linked list representation
object nth {

  def nth[T](n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException("element does not exists")
    else if (n == 0) list.head
    else nth(n - 1, list.tail)                    //> nth: [T](n: Int, list: week3_polymorphism.List[T])T

  val list = new Cons(0, new Cons(1, new Cons(2, Nil)))
                                                  //> list  : week3_polymorphism.Cons[Int] = week3_polymorphism.Cons@b684286
  val x = nth(5, list)                            //> java.lang.IndexOutOfBoundsException: element does not exists
                                                  //| 	at week3_polymorphism.nth$$anonfun$main$1.nth$1(week3_polymorphism.nth.s
                                                  //| cala:16)
                                                  //| 	at week3_polymorphism.nth$$anonfun$main$1.apply$mcV$sp(week3_polymorphis
                                                  //| m.nth.scala:21)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3_polymorphism.nth$.main(week3_polymorphism.nth.scala:13)
                                                  //| 	at week3_polymorphism.nth.main(week3_polymorphism.nth.scala)
}