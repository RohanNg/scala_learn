package week6_COLLECTIONS

/**
 * 	Most operations on sequences are available on set
 *
 * 	Fundamental operation: contains, deletes, add
 */

object sett {
  /**
   * Solution to nQueen problem:
   * 		Given a board of dim n-by-n
   * 		Figure out how to place n queens on the board
   * 		so that no queens is threatened by others
   */
  def nQueens(n: Int): Set[List[Int]] = {
    def placeQueen(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueen(k - 1) //for: transforming the first generator in an understandable way
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
    }
    /**
     * @param col column of the candidate queen
     * @param queens list of columns position of the queens
     * (the first element in list coresponds to the col position of the queen in row (queens.length - 1)
     * 	and the last element in list corresponds to the col position of the queen in row 0)
     */
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length // row of the candidate queen
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => col != c && (math.abs(col - c) != row - r)
      }
    }
    placeQueen(n)
  }                                               //> nQueens: (n: Int)Set[List[Int]]

  def showQueens(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse) // transforming queesn.reverse
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }                                               //> showQueens: (queens: List[Int])String

  (nQueens(8) take 4  map showQueens) mkString "\n"
                                                  //> res0: String = "
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * X * * 
                                                  //| 
                                                  //| * * * * * X * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * X * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| 
                                                  //| * X * * * * * * 
                                                  //| * * * * * X * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * X * * * * * 
                                                  //| * * * * X * * * "


}