package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Наименьший из 2 элементов должен быть самым маленьким в ранее пустой куче.") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("Удаление минимального значения в куче должно привести к пустой куче") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("Рекусивный поиск и удаление элементов в куче должны возвращать те же элементы") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val t_h = deleteMin(h)
        isEmpty(t_h) || (findMin(h) <= findMin(t_h) && isSorted(t_h))
      }
    isSorted(h)
  }

  property("Минимальное значение объединенной кучи должно быть минимальным от минимума обеих куч.") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }


  property("Две кучи должны быть равны, если рекурсивное удаление минимальных элементов приводит к тем же элементам, пока они не станут пустыми") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        findMin(h1) == findMin(h2) && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("Минимальное значение 2 куч должно быть минимальным после перемещения их из кучи 1 в 2 и объединения обоих") = forAll { (h1: H, h2: H) =>
    val m = min(findMin(h1), findMin(h2))
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }
}
