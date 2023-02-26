package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty), 
    for 
      i<-arbitrary[Int] 
      h<-oneOf(const(empty), genHeap)
    yield insert(i, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll{ (a: Int, b: Int) => 
    val h = meld(insert(a, empty), insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("empty") = forAll{ (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  val nonEmptyGen2 = 
    for h1<-arbitrary[H]
        h2<-arbitrary[H]
        if !isEmpty(h1) && !isEmpty(h2)
    yield (h1, h2)

  property("minHeap") = forAll(nonEmptyGen2){ (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  property("empty") = forAll{
    (h: H)=>if !isEmpty(h) then findMin(meld(h, empty)) == findMin(h) else true
  }

  property("insert") = forAll{
    (a: Int, h: H) => !isEmpty(insert(a, h))
  }

  property("findDelete") = forAll{
    (a: Int, b: Int) =>
      findMin(deleteMin(meld(insert(a, empty), insert(b, empty)))) == Math.max(a,b)
  }

  property("remainOrder") = forAll(for a<-arbitrary[Int] if a<Int.MaxValue-2 yield a){
    (a: Int) =>
      val h = (for i<- a to a+2 yield insert(i, empty)).reduce(meld)
      findMin(deleteMin(h)) == a+1
  }