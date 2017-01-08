package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1,3)
    val goal = Pos(5,8)
  }
  // this solution will cause stackOverflow
  //println(InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain
   */
  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |------
        |------
        |------""".stripMargin
  }

  println(Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------oooooo-----
        |oSooo-----o--ooo--
        |oo-oo-oooooo-------
        |-ooooo---oooo
        |----ooooooooooooooooooT
        |------ooo-""".stripMargin
  }

  println(Level1.solution)
}
