import quickcheck._

object propsTest {
	val h = new QuickCheckHeap with BinomialHeap

/*  val heap = h.genHeap.sample.get
  val list = h.heapToList(heap)
  list.sorted */

  val l = h.genList.sample.get


  l.min

  val heap = h.listToHeap(l)





  h.findMin(heap)
}



