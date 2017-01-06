package week6_COLLECTIONS


object collection {
	// Sequen :> List :> Vector
	/**
	*		Scala collection lib
	*														Iterable
	*					Seq									Set 				Map
	* List    Vector	Range
	*		(String Array)
	*
  *	Sequence ops: map, flatMap, flatten => FOR expression
  *								filter, filterNot, partition,
  *								takeWhile, dropWhile, span
  *							 	forall; exists
  *								head, tail, init, last
  *								foldLeft, reduceLeft
  *								zip, unzip    => make list of tuple
  *								flatMap f     == (xs map f).flatten
	*								sum, max, min
	*								updated
	*								sorted, sortWith
	*								gruopBy
	*/
	
	// Array behaves like sequence
	val y = Array(1,2,3)                      //> y  : Array[Int] = Array(1, 2, 3)
	y map (_ + "zzz")                         //> res0: Array[String] = Array(1zzz, 2zzz, 3zzz)
	y exists (_ == 2)                         //> res1: Boolean = true
	
	// String behave like sequence
	val x = "Nguyen Hai Dang"                 //> x  : String = Nguyen Hai Dang
	x.partition(_.isUpper)                    //> res2: (String, String) = (NHD,guyen ai ang)
	x exists ( _ == 'a')                      //> res3: Boolean = true
	x exists ( _ == "a")                      //> res4: Boolean = false
	x map ((x) => x + "__")                   //> res5: scala.collection.immutable.IndexedSeq[String] = Vector(N__, g__, u__, 
                                                  //| y__, e__, n__, " __", H__, a__, i__, " __", D__, a__, n__, g__)
	// Range example
	val r1 = 1 until 5                        //> r1  : scala.collection.immutable.Range = Range(1, 2, 3, 4)
	val r2 = 1 to 5                           //> r2  : scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5)
	val r3 = 1 until 10 by 3                  //> r3  : scala.collection.immutable.Range = Range(1, 4, 7)
	val r4 = 6 to 1 by -2                     //> r4  : scala.collection.immutable.Range = Range(6, 4, 2)
	
	1 to 3 zip "abcd"                         //> res6: scala.collection.immutable.IndexedSeq[(Int, Char)] = Vector((1,a), (2,
                                                  //| b), (3,c))
	r1 map ((x) => x*x)                       //> res7: scala.collection.immutable.IndexedSeq[Int] = Vector(1, 4, 9, 16)
	
	// Vector example
	val v1 =  Vector(1,2,3,5,6,7,8)           //> v1  : scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 5, 6, 7, 8)
	
	"abcd" flatMap (c => List('.', c))        //> res8: String = .a.b.c.d
	
	(1 to 4) flatMap (x => (4 to 8) map (y => (x,y)))
                                                  //> res9: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,4), (1,
                                                  //| 5), (1,6), (1,7), (1,8), (2,4), (2,5), (2,6), (2,7), (2,8), (3,4), (3,5), (
                                                  //| 3,6), (3,7), (3,8), (4,4), (4,5), (4,6), (4,7), (4,8))
 	
}