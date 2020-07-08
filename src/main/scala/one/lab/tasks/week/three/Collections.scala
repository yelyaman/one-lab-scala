package one.lab.tasks.week.three

object Collections extends App{

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    def innerDuplicate(count: Int, acc: List[A], curList: List[A]): List[A] = curList match {
      case Nil => acc
      case _ :: xs if count == 0 => innerDuplicate(count = n, acc, xs)
      case x :: _ => innerDuplicate(count - 1 , acc :+ x, curList)
    }
    innerDuplicate(n, List.empty[A], list)
  }


  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))
  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    def innerSplit(counter: Int, acc: List[A], curlist: List[A]): (List[A], List[A]) = curlist match {
      case _ if counter == 0 => (acc, curlist)
      case x :: xs => innerSplit(counter - 1, acc :+ x, xs)
    }
    innerSplit(k, List.empty[A], list)
  }

  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 3)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) =  {
    def innerRemove(counter: Int, acc: List[A], curlist: List[A]): (List[A], A) = curlist match {
      case Nil if counter >= 0 => throw new java.lang.NullPointerException()
      case _ if counter < 0 => throw new java.lang.NullPointerException()
      case x :: xs if counter == 0 => (acc ++ xs, x)
      case x :: xs => innerRemove(counter - 1, acc :+ x, xs)
    }
    innerRemove(k, List.empty[A], list)
  }
}
