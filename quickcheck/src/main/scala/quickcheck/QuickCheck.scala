package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {



  lazy val genHeap: Gen[H] = for{
    x <- arbitrary[A]
    y <- oneOf(const(empty),genHeap)
  } yield insert(x,y)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert two elements and the findMin should give minimum of tww") = forAll { (a: Int, b:Int)=>
    val h1 = insert(a,empty)
    val h2 = insert(b,h1)
    findMin(h2) == math.min(a,b)
  }


  property("Insert into empty heap and then delete should result in empty heap") = forAll { a:Int =>
    val h = insert(a,empty)
    val deletedHeap = deleteMin(h)
    isEmpty(deletedHeap)
  }


  property("get sorted sequence continuosly finding and deleting minimum") = forAll{ h:H =>

    def getElems(heap: H): List[A] ={
      isEmpty(heap) match {
        case true => Nil
        case false => {
          val m = findMin(heap)
          m :: getElems(deleteMin(heap))
        }
      }
    }

    val elems = getElems(h)
    elems.sorted == elems

  }

  property("finding minimum of melding of two heaps should give either one") = forAll { (h1:H, h2:H) =>

    val combinedMin = findMin(meld(h1,h2))

    (combinedMin == findMin(h1)) || (combinedMin == findMin(h2))

  }

  property("sorted") = Prop.forAll { (h: H) =>
    def isSorted(last: Int, heap: H): Boolean = {
      isEmpty(heap) || {
        val min = findMin(heap)
        last <= min && isSorted(min, deleteMin(heap))
      }
    }

    isEmpty(h) || isSorted(findMin(h), deleteMin(h))
  }

  property("minOfSome") = Prop.forAll { (h1: H, h2: H) =>
    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
    val m = findMin(meld(h1, h2))

    m == m1 || m == m2
  }

  property("Finding a minimum of the melding of a heap with an empty heap should return the minimum of the heap.") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, empty)
      val h2 = meld(empty, h)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      (m == m1) && (m == m2) && (m1 == m2)
    }

  property("Finding a minimum of the melding of any two identical heaps should return a minimum of one or the other.") =
    forAll { (h: H) =>
      val m = findMin(h)
      val h1 = meld(h, h)
      val m1 = findMin(h1)
      (m == m1)
    }


  property("find min of 2 arbitrary heaps") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    isEmpty(h1) || isEmpty(h2) || min == findMin(h1) || min == findMin(h2)
  }

  property("finding and deleting minimums of arbitrary heap") = forAll { h: H =>
    def reduce(h: H): List[A] =
      if (!isEmpty(h)) findMin(h) :: reduce(deleteMin(h))
      else Nil
    reduce(h) == reduce(h).sorted
  }

  property("delete min and insert it again") = forAll { h: H =>
    val reinserted = insert(findMin(h), deleteMin(h))
    findMin(reinserted) == findMin(h)
  }

  property("transferring the minimum to another heap") = forAll { (h1: H, h2: H) =>
    def reduce(h: H): List[A] =
      if (!isEmpty(h)) findMin(h) :: reduce(deleteMin(h))
      else Nil
    val melded = meld(h1, h2)
    val transferred = meld(deleteMin(h1), insert(findMin(h1), h2))
    reduce(melded) == reduce(transferred)
  }



}

