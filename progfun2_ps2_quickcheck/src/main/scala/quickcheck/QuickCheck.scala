package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(genHeap, const(empty))
  } yield insert(x, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /*Create a list in ascending order by sequences of (findMin an deleteMin) */
  private def createList(h: H): List[A] =
    if(isEmpty(h)) Nil
    else
      findMin(h) :: createList(deleteMin(h))

  /* insert all element from a list into a heap */
  private def insertInto(h: H, xs: List[A]): H =
    (xs foldLeft h) ((heap: H, ele: A) => insert(ele, heap))

  // warm up test
  property("empty heap should be empty") = forAll {
    (_: A) => isEmpty(empty)
  }

  // Strongest Test
  property("inserting many elements into an empty heap") = forAll {
    (xs: List[A]) =>
      if(xs.isEmpty) true
      else {
        val sortedXs = xs.sorted
        val testHeap =  insertInto(empty, xs)
        findMin(testHeap) == sortedXs.head && createList(testHeap).sorted == xs.sorted
      }
  }

  // If the above test failed, one may use the following test to debug

  //  property("insert into empty heap, then delete it") = forAll {
  //    (n: A) =>
  //      val heapWithOneElement = insert(n, empty)
  //      !isEmpty(heapWithOneElement) &&
  //        findMin(heapWithOneElement) == n &&
  //        isEmpty(deleteMin(heapWithOneElement))
  //  }
  //
  //  property("findMin and deleteMin should create an ascending list") = forAll {
  //    (h: H) => {
  //      val elemList: List[A] = createList(h)
  //      elemList.sorted == elemList
  //    }
  //  }
  //
  //  property("insert min of a heap into a heap") = forAll {
  //    (h: H) =>
  //      val m = if (isEmpty(h)) 0 else findMin(h)
  //      findMin(insert(m, h)) == m
  //  }

  // Testing meld
  property("meld two heap") = forAll {
    (h1: H, h2: H) =>
      val melded = meld(h1,h2)
      if(isEmpty(h1)) {
        if(isEmpty(h2)) isEmpty(melded)
        else if (isEmpty(melded)) false
        else findMin(h2) == findMin(melded)
      } else if(isEmpty(h2)) {
        if(isEmpty(melded)) false
        else findMin(h1) == findMin(melded)
      } else if (isEmpty(melded)) {
        false
      }
      else findMin(melded) == math.min(findMin(h1), findMin(h2))
  }
}
