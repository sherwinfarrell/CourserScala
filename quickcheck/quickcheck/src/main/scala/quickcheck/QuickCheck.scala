package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    randElem <- arbitrary[A]
    randHeap <- oneOf(const(empty), genHeap)
  } yield insert(randElem, randHeap)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("minOF2") = forAll {(a: A, b:A) =>
      val randHeap = insert(a, empty)
    val randHeap2 = insert(b, randHeap)
    val minOfHeap = findMin(randHeap2)
    if (a <= b) {
      minOfHeap== a
    }
    else {
      minOfHeap == b
    }  }




  property("Deleting min element in a sinlge element heap tree should give empty heap") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }


  property("sorted heap after finding and deleting") = forAll{ (h: H) =>
    def checkSort(h: H): Boolean = {
      if(isEmpty(h)) true
      else {
        val min = findMin(h)
        val newHeap = deleteMin(h)
        ( isEmpty(newHeap) || min <= findMin(newHeap) && checkSort(newHeap))
      }
    }
    checkSort(h)

  }

  property("mininum of meddling 2 heaps") = forAll{ (h1: H, h2: H) =>
     if(findMin(h1)<= findMin(h2)){
       val minVal = findMin(h1)
       minVal == findMin(meld(h1, h2))
     }
     else {val minVal = findMin(h2)
       minVal == findMin(meld(h1, h2))
     }

  }


  property("Removing and adding elementing to other heap and melding should still give the same min result") = forAll { (h1: H, h2: H) =>
    def isEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && isEqual(deleteMin(h1), deleteMin(h2))
      }
    isEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }


}
