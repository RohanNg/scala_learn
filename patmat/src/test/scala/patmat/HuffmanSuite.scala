package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Huffman.frenchCode
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3), ('m', 5))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3), Leaf('m',5)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createNodeTree of some char list") {
    assert(createCodeTree(("aaabbc").toList) === Fork(Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3),Leaf('a',3),List('c', 'b', 'a'),6))
    assert(createCodeTree(("1223334444").toList) === Fork(Leaf('4',4),Fork(Fork(Leaf('1',1),Leaf('2',2),List('1', '2'),3),Leaf('3',3),List('1', '2', '3'),6),List('4', '1', '2', '3'),10))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)("nguyenhaidang".toList)) === "nguyenhaidang".toList)
      assert(decode(frenchCode, encode(frenchCode)("qwertyuiopasdfghjklzxcvbnm".toList)) === "qwertyuiopasdfghjklzxcvbnm".toList)
    }
  }

}
