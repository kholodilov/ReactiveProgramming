package quickcheck

object heapsTest {
  val h = new BinomialHeap with IntHeap           //> h  : quickcheck.BinomialHeap with quickcheck.IntHeap = quickcheck.heapsTest$$
                                                  //| anonfun$main$1$$anon$1@6c87080f
  val heap0 = h.empty                             //> heap0  : scala.collection.immutable.Nil.type = List()
  h.isEmpty(heap0)                                //> res0: Boolean = true
  
  val heap1 = h.insert(1, heap0)                  //> heap1  : quickcheck.heapsTest.h.H = List(Node(1,0,List()))
  h.isEmpty(heap1)                                //> res1: Boolean = false
  h.findMin(heap1)                                //> res2: quickcheck.heapsTest.h.A = 1
  
  val heap2 = h.insert(2, heap1)                  //> heap2  : quickcheck.heapsTest.h.H = List(Node(1,1,List(Node(2,0,List()))))
  h.findMin(heap2)                                //> res3: quickcheck.heapsTest.h.A = 1

	val heap3 = h.insert(0, heap2)            //> heap3  : quickcheck.heapsTest.h.H = List(Node(0,0,List()), Node(1,1,List(Nod
                                                  //| e(2,0,List()))))
	h.findMin(heap3)                          //> res4: quickcheck.heapsTest.h.A = 0
	h.findMin(h.deleteMin(heap3))             //> res5: quickcheck.heapsTest.h.A = 1
	
	val heap4 = h.insert(-1, h.insert(3, heap0))
                                                  //> heap4  : quickcheck.heapsTest.h.H = List(Node(-1,1,List(Node(3,0,List()))))
  h.findMin(heap4)                                //> res6: quickcheck.heapsTest.h.A = -1
	val heap5 = h.meld(heap3, heap4)          //> heap5  : quickcheck.heapsTest.h.H = List(Node(0,0,List()), Node(-1,2,List(No
                                                  //| de(1,1,List(Node(2,0,List()))), Node(3,0,List()))))
  h.findMin(heap5)                                //> res7: quickcheck.heapsTest.h.A = -1
	h.findMin(h.deleteMin(heap5))             //> res8: quickcheck.heapsTest.h.A = 0
	h.findMin(h.deleteMin(h.deleteMin(heap5)))//> res9: quickcheck.heapsTest.h.A = 1
}