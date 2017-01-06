package week5_List

// tuples : generalization of pair

// tuple pattern: scale.Tuplen[T1, T2, ... Tn])(t1: T1, t2: T2, ... tn: Tn)

object pairs_tuples {

  /**
   * Perform insertion sort on a list of Ints
   */
  def iSort(xs: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List() => List(x)
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }
 
    xs match {
      case List() => List()
      case y :: ys => insert(y, iSort(ys))
    }
  }                                               //> iSort: (xs: List[Int])List[Int]

  iSort(List(1, 5, 2, 4, 3))                      //> res0: List[Int] = List(1, 2, 3, 4, 5)
	
	
	
	
  //one may use (implicit ord: Ordering) instead of (lessThan: (T, T) => Boolean )
  //		ord.lt(a,b) <=> lessThan(a,b)
  
  /**
  *	Merge sort algorithms
  *
  */
  def msort[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2

    if (n == 0) xs
    else {

      /* not elegant version wihout using tuples
      def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 => ys match {
          case Nil => xs
          case y :: ys1 => if (x <= y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }
      }*/

      def merge(l: List[T], r: List[T]): List[T] = (l, r) match {
        case (Nil, _) => r
        case (_, Nil) => l
        case (x :: ll, y :: rr) =>
          if (lessThan(y, x)) y :: merge(l, rr)
          else x :: merge(ll, r)
      }

      val (l, r) = xs splitAt n
      merge(msort(l)(lessThan), msort(r)(lessThan))
    }
  }                                               //> msort: [T](xs: List[T])(lessThan: (T, T) => Boolean)List[T]

	
	/**
	*TUPLES
	*
	*/
  val tup = ("dang", 1997)                        //> tup  : (String, Int) = (dang,1997)
	
	//decompose
  val (name, date) = tup                          //> name  : String = dang
                                                  //| date  : Int = 1997
  //decompose with pattern matching
  tup match {
  	case ("dang", date) => true
  }                                               //> res1: Boolean = true
  //decompose by method
  tup._1 == name                                  //> res2: Boolean = true
  tup._2 == date                                  //> res3: Boolean = true
  
  msort(List(1, 7, 2, 4, 6, 9, 10))(_ < _)        //> res4: List[Int] = List(1, 2, 4, 6, 7, 9, 10)

  msort(List("b", "a", "f", "d", "c"))((x, y) => x.compareTo(y) < 0)
                                                  //> res5: List[String] = List(a, b, c, d, f)
}