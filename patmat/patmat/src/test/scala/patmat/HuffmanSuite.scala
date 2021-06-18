package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `tryign to make code tree`: Unit =
    new TestTrees {
      assertEquals(Fork(t1, t2, List('a' , 'b' ,'a','b', 'd'), 14),makeCodeTree(t1, t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `times for the gives chars in the list`: Unit =
    new TestTrees {
      assertEquals(List(('c',1), ('b',2), ('a',1)),times(List('a' , 'b', 'b', 'c')))
    }


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)),

      combine(leaflist))
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `code Table Test`: Unit = new TestTrees {
    val codeTable = List(('a', List(1,0,1)), ('b', List(2,0,2)))
    assertEquals(List(2,0,2), codeBits(codeTable)('b'))
  }

  @Test  def `conver test`: Unit = new TestTrees {
    println(convert(t2))
  }

  @Test  def `QUick encode test`: Unit = new TestTrees {
    println(quickEncode(t2)(List('a','b','d')))
  }
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
