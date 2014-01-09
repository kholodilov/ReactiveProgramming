import quickcheck._

object heapsTest {
    val h = new BinomialHeap with IntHeap

    val heap0 = h.empty
    h.isEmpty(heap0)

    val heap1 = h.insert(1, heap0)
    h.isEmpty(heap1)
    h.findMin(heap1)

    val heap2 = h.insert(2, heap1)
    h.findMin(heap2)

    val heap3 = h.insert(0, heap2)

    h.findMin(heap3)
    h.findMin(h.deleteMin(heap3))

    val heap4 = h.insert(-1, h.insert(3, heap0))

    h.findMin(heap4)
    val heap5 = h.meld(heap3, heap4)


    h.findMin(heap5)
    h.findMin(h.deleteMin(heap5))
    h.findMin(h.deleteMin(h.deleteMin(heap5)))
}

