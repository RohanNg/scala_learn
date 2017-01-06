package week5_List

object higher_order_list_funcs {
  // Testing forAlls, exists
  /**
   * 	FLATTEN a list structure
   */

  def flattenDeep(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case y :: ys => y match {
      case List() => flattenDeep(ys)
      // case y is a non empty list
      case _ :: _ => flattenDeep(y.asInstanceOf[List[Any]]) ::: flattenDeep(ys)
      case t => t :: flattenDeep(ys)
    }
  }                                               //> flattenDeep: (xs: List[Any])List[Any]

  flattenDeep(List(List(List(), 1, 1), List(), 2, List(3, List(List(List("a")), 5, 8))))
                                                  //> res0: List[Any] = List(1, 1, 2, 3, a, 5, 8)
	
	def flatten(xs: List[List[Any]]): List[Any] = (xs foldRight List[Any]())(_++_)
                                                  //> flatten: (xs: List[List[Any]])List[Any]
	
	
  // MAPPING pattern is genalized in List.map
  def squareListV1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => (y * y) :: squareListV1(ys)
  }                                               //> squareListV1: (xs: List[Int])List[Int]

  // under stand the simple thing List.map can do
  def simpleMap[U, T](xs: List[T])(f: T => U): List[U] = xs match {
    case Nil => List()
    case y :: ys => f(y) :: simpleMap(ys)(f)
  }                                               //> simpleMap: [U, T](xs: List[T])(f: T => U)List[U]

  // use simple map
  def squareListV2(xs: List[Int]): List[Int] =
    simpleMap(xs)((x: Int) => x * x)              //> squareListV2: (xs: List[Int])List[Int]

  // use list map
  def squareListV3(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareListV3: (xs: List[Int])List[Int]

  simpleMap(List(1, 2, 3, 4))((x: Int) => x * x)  //> res1: List[Int] = List(1, 4, 9, 16)

  squareListV1(List(1, 2, 3))                     //> res2: List[Int] = List(1, 4, 9)

  squareListV3(List(1, 2, 3, 4, 5, 6))            //> res3: List[Int] = List(1, 4, 9, 16, 25, 36)

  // FILTERING pattern is generalized in List.filter

  def posElemV1(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElemV1(ys) else posElemV1(ys)
  }                                               //> posElemV1: (xs: List[Int])List[Int]

  def posElemV2(xs: List[Int]) =
    xs filter (x => x > 0)                        //> posElemV2: (xs: List[Int])List[Int]

  posElemV2(List(-1, 2, 0, -4))                   //> res4: List[Int] = List(2)

  // FILTER VARIATION of filter

  val a = List(-1, 4, 3, 0, 2, -5)                //> a  : List[Int] = List(-1, 4, 3, 0, 2, -5)

  a filter (x => x > 0)                           //> res5: List[Int] = List(4, 3, 2)
  a filterNot (x => x > 0)                        //> res6: List[Int] = List(-1, 0, -5)
  a partition (x => x > 0)                        //> res7: (List[Int], List[Int]) = (List(4, 3, 2),List(-1, 0, -5))

  a takeWhile (x => x != 0)                       //> res8: List[Int] = List(-1, 4, 3)
  a dropWhile (x => x != 0)                       //> res9: List[Int] = List(0, 2, -5)
  a span (x => x != 0)                            //> res10: (List[Int], List[Int]) = (List(-1, 4, 3),List(0, 2, -5))

  // PACK
  val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
  /**
   * 	PACK consecutive element into a list
   */
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (same, diff) = xs span (xx => x == xx)
      same :: pack(diff)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  pack(data)                                      //> res11: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a
                                                  //| ))
  // ENCODE
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))     //> encode: [T](xs: List[T])List[(T, Int)]

  encode(data)                                    //> res12: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  // COMBINATION PATTERN, reduction on list: foldLeft, reduceLeft

  val data2 = List(1, 2, 3, 4, 5, 6, 7)           //> data2  : List[Int] = List(1, 2, 3, 4, 5, 6, 7)

  (0 :: data2) reduceLeft (_ + _)                 //> res13: Int = 28
  (data2 foldLeft 0)(_ + _)                       //> res14: Int = 28
  (1 :: data2) reduceLeft (_ * _)                 //> res15: Int = 5040
  (data2 foldLeft 1)(_ * _)                       //> res16: Int = 5040

  def reduceRight[T](xs: List[T])(op: (T, T) => T): T = xs match {
    case Nil => throw new Error("Nil.reduceRight")
    case x :: Nil => x
    case x :: xs => op(x, reduceRight(xs)(op))
  }                                               //> reduceRight: [T](xs: List[T])(op: (T, T) => T)T

  def foldRight[U, T](xs: List[T])(z: U)(op: (T, U) => U): U = xs match {
    case Nil => z
    case y :: ys => op(y, (foldRight(ys)(z)(op)))
  }                                               //> foldRight: [U, T](xs: List[T])(z: U)(op: (T, U) => U)U

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)                     //> concat: [T](xs: List[T], ys: List[T])List[T]

  concat(List(1, 2), List("a", "b"))              //> res17: List[Any] = List(1, 2, a, b)

  def mapFun[T, U](xs: List[T])(f: T => U): List[U] =
    (xs foldRight List[U]())((x: T, list: List[U]) => f(x) :: list)
                                                  //> mapFun: [T, U](xs: List[T])(f: T => U)List[U]

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, x: Int) => x + 1)        //> lengthFun: [T](xs: List[T])Int

  lengthFun(data2)                                //> res18: Int = 7
}