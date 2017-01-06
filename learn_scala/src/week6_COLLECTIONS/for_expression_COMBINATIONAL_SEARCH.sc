package week6_COLLECTIONS
/**
 * for expression: combines map, filter, flatten or flatMap
 *
 */
object combinatorial_search {
  def isPrime(n: Int): Boolean = (2 to Math.sqrt(n).toInt) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean
  def generatePair(n: Int) = (2 until n) flatMap (j => (1 until j) map (i => (j, i)))
                                                  //> generatePair: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
	
  // anser to question: given and, find pair of number i,j such as 1<= i < j < n and i +j is a prime number
  def generatePairSumToPrime(n: Int) = generatePair(n) filter (pair => isPrime(pair._1 + pair._2))
                                                  //> generatePairSumToPrime: (n: Int)scala.collection.immutable.IndexedSeq[(Int, 
                                                  //| Int)]
  def generatePairSumToPrimeV2(n: Int) =
    for {
      i <- 2 until n // generator taken element from collection
      j <- 1 until i // generator, if it is an empty collection, skip yield
      if isPrime(i + j) // conditioner
    } yield (i, j) // selector resultihg in a collection
                                                  //> generatePairSumToPrimeV2: (n: Int)scala.collection.immutable.IndexedSeq[(Int
                                                  //| , Int)]

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    if (xs.length == ys.length) ((xs zip ys) map { case (x, y) => x * y }).sum // short hand for map equal (x => x match {})
    else throw new Error("Two vector of different dimension")
                                                  //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double
                       
  def scalarProductV2(xs: Vector[Double], ys: Vector[Double]): Double =
  	(for ((x,y) <- xs zip ys) yield x*y).sum  //> scalarProductV2: (xs: Vector[Double], ys: Vector[Double])Double
    
  scalarProductV2(Vector(1,2,3), Vector(1,2,3))   //> res0: Double = 14.0
}