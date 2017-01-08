package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("StringParserTerrain terrainFunction, findChar should work correctly") {
    new Level1 {
      val levelVec = Vector(Vector('-','-' ,'-','-' ),Vector('-','S', 'T','-'), Vector('-','o', 'z','-'), Vector('-','o', 'o','-'))
      val terrainFunc = terrainFunction(levelVec)
      assert(!terrainFunc(Pos(1,0)) ,"1,0")
      assert(!terrainFunc(Pos(0,2)) ,"0,2")
      assert(terrainFunc(Pos(1,1)) ,"1,1")
      assert(terrainFunc(Pos(2,1)) ,"2,1")

      // findChar
      assert(findChar('S', levelVec) == Pos(1,1), "char S should be at Pos(1,1)")
      assert(findChar('T', levelVec) == Pos(1,2), "char T should be at Pos(1,2)")
      assert(findChar('z', levelVec) == Pos(2,2), "char T should be at Pos(2,2)")
    }
  }

  test("neighborWithHistory on level 1") {
    new Level1 {
      val neiStream = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val expectedNeiSet = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assert(neiStream.toList.toSet === expectedNeiSet, "expected neighbor of the start block" )
    }
  }

  test("newNeighborOnly on level 1") {
    new Level1 {
      val neighbours: Stream[(Block, List[Move])] = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val visitedNeighbor = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))

      val unvisitedNeibor = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))

      assert(newNeighborsOnly(neighbours, visitedNeighbor).toSet ===  unvisitedNeibor)
    }
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
