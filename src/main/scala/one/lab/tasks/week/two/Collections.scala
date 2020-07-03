package one.lab.tasks.week.two

object Collections {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new java.util.NoSuchElementException
    case x :: Nil => x
    case _ => getLast(list.tail)
  }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ => getLastOption(list.tail)
  }

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = list match {
    case Nil | _ :: Nil => throw new java.util.NoSuchElementException
    case x :: _ :: Nil => x
    case _ => getPreLast(list.tail)
  }

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] = list match {
    case Nil | _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case _ => getPreLastOption(list.tail)
  }

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A = list match {
    case Nil => throw new java.lang.IndexOutOfBoundsException()
    case _ if n == 1 => list.head
    case _ => getNthElement(n - 1, list.tail)
  }

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = list match {
    case Nil => None
    case _ if n == 1 => Some(list.head)
    case _ => getNthElementOption(n - 1, list.tail)
  }

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    def innerLength(count: Int, list: List[A]): Int = list match {
      case Nil => count
      case _ => innerLength(count + 1, list.tail)
    }
    innerLength(0, list)
  }

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    def innerReversed(acc: List[A], curList: List[A]): List[A] = curList match {
      case Nil => acc
      case x :: xs => innerReversed( x +: acc, xs)
    }
    innerReversed(List.empty[A], list)
  }

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    def innerDuplicate(acc: List[A], curList: List[A]): List[A] = curList match {
      case Nil => acc
      case x :: xs => innerDuplicate(acc :+ x :+ x, xs)
    }
    innerDuplicate(List.empty[A], list)
  }
}
