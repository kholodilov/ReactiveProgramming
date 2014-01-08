package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insertAndDeleteResultsEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("minInsertFirst") = forAll { (a: Int, b: Int) =>
    (a <= b) ==> {
      val h = insert(b, insert(a, empty))
      findMin(h) == a
    }
  }

  property("minInsertLast") = forAll { (a: Int, b: Int) =>
    (a <= b) ==> {
      val h = insert(a, insert(b, empty))
      findMin(h) == a
    }
  }

  property("sortedOutput") = forAll { (h: H) =>
     val list = heapToList(h)
     list == list.sorted
  }  
  
  property("meldMin") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2
    val h3 = meld(h1, h2)
    findMin(h3) == min
  }  
  
  property("meldWithEmptyLeft") = forAll { (h: H) =>
    val min = findMin(h)
    val hh = meld(empty, h)
    findMin(hh) == min
  }  

  property("meldWithEmptyRight") = forAll { (h: H) =>
    val min = findMin(h)
    val hh = meld(h, empty)
    findMin(hh) == min
  }  

  property("insertFromListMin") = forAll { l: List[A] =>
    (!l.isEmpty) ==> {
      val h = listToHeap(l)
      findMin(h) == l.min
    }
  }

  property("insertFromListEquals") = forAll { l: List[A] =>
    (!l.isEmpty) ==> {
      val h = listToHeap(l)
      val ll = heapToList(h)
      l.sorted == ll
    }
  }
  
  def heapToList(h: H): List[A] = h match {
    case h if isEmpty(h) => Nil
    case _ => findMin(h) :: heapToList(deleteMin(h))
  }

  def listToHeap(l: List[A]): H = l match {
    case Nil => empty
    case head :: tail => insert(head, listToHeap(tail))
  }
  
  lazy val genNonEmptyHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- genNonEmptyHeap | genEmptyHeap
  } yield insert(a, h)
  lazy val genEmptyHeap: Gen[H] = empty
  lazy val genHeap: Gen[H] = genNonEmptyHeap
  
  lazy val genList: Gen[List[A]] = Gen.containerOf[List, A](Gen.choose(Int.MinValue, Int.MaxValue))
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  implicit lazy val arbList: Arbitrary[List[A]] = Arbitrary(genList)

}
