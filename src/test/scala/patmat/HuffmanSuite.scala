package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  test("weight of a larger tree 2") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("times aba = 2, 1") {
    val t = times(List('a', 'b', 'a'))
    //    println(t)
    assert((t) == List(('a', 2), ('b', 1)))
  }



  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val c = combine(leaflist)
    //   println(c)
    assert(c === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }
/*  test("createCodeTree") {
    val l = createCodeTree(string2Chars("test"))
 //   println(l)
    assert(l == Fork(Fork(Leaf('e', 1), Leaf('s', 1), List('e', 's'), 2), Leaf('t', 2), List('e', 's', 't'), 4))
  }
*/
  test("frenchcode huffmanestcool") {
    val d = decodedSecret
    assert(d.mkString == "huffmanestcool")
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val en = encode(t1)("ab".toList)
      assert(decode(t1, en) === "ab".toList)
    }
  }
  test("decode and encode frenchcode") {
    new TestTrees {
      val en = encode(frenchCode)("huffmanestcool".toList)
      assert(en == secret)
    }
  }
  test("decode and quick encode frenchcode") {
    new TestTrees {
      val en = quickEncode(frenchCode)("huffmanestcool".toList)
      assert(en == secret)
    }
  }


}
