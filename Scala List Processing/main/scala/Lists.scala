object Lists {

  val oddNumbers = 1 :: 3 :: 5 :: Nil
  val evenNumbers = 2 :: 4 :: 6 :: Nil
  val oddsEvens = 1 :: 2 :: 3 :: 4 :: Nil
  val zeros = 0 :: 2 :: 1 :: 0 :: Nil
  val wrong = 5 :: 6 :: 2 :: 3 :: 1 :: Nil

  def sumDouble(ints: List[Int]): Int = ints match{
    case Nil => 0
    case head :: tail=> 2*head + sumDouble(tail)
  }
  def allGTE(n: Int, ints: List[Int]): List[Int] = {
    ints match {
      case Nil => Nil
      case head :: tail => {
        if(head >= n) {
          head :: allGTE(n, tail)
        }
        else {
          allGTE(n, tail)
        }
      }
    }
  }
  def allLT(n: Int, ints: List[Int]): List[Int] = {
    ints match {
      case Nil => Nil
      case head :: tail => {
        if(head < n) {
          head :: allLT(n, tail)
        }
        else {
          allLT(n, tail)
        }
      }
    }
  }
  def countEvens(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case head :: tail => {
        if(head%2 == 0){
          1 + countEvens(tail)
        }
        else{
          countEvens(tail)
        }
      }
    }
  }
  def addSub(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case head :: Nil => head
      case head :: x :: tail => head - x + addSub(tail)
    }
  }
  def removeZeros(ints: List[Int]): List[Int] = {
    ints match {
      case Nil => Nil
      case 0 :: tail  => removeZeros(tail)
      case head :: tail => head :: removeZeros(tail)
    }
  }
  def removeAlternating(ints: List[Int]): List[Int] = {
    ints match {
      case Nil => Nil
      case head :: Nil => head :: Nil
      case head :: x :: tail => head :: removeAlternating(tail)
    }
  }
  def remove(x: Int, ints: List[Int]): List[Int] = {
    ints match {
      case Nil => Nil
      case head :: y :: tail  => {
        if(y == x){
          head :: tail
        }
        else {
          head :: remove(x, y :: tail)
        }
      }
      case head :: tail => head :: remove(x, tail)
    }
  }
  def findWrongNumber(ints: List[Int]): Int = {
    ints match{
      case Nil => -1
      case head :: Nil => -1
      case head :: x :: tail => {
        if(x < head) {
          x
        }
        else {
          findWrongNumber(x :: tail)
        }
      }
    }
  }
  def sort(ints: List[Int]): List[Int] = {
    (isAscending(ints)) match {
      case true => ints
      case false => {
        val x = findWrongNumber(ints)
        sort(insertOrdered(x, remove(x, ints)))
      }
    }
  }
  def isAscending(ints: List[Int]): Boolean = {
    ints match {
      case Nil => true
      case head :: Nil => true
      case head :: x :: tail => {
        if(head > x) {
          false
        }
        else {
          isAscending(x :: tail)
        }
      }
    }
  }
  def fromTo(lo: Int, hi: Int): List[Int] = {
    (lo == hi) match {
      case true => Nil
      case false => lo :: fromTo(lo+1, hi)
    }
  }
  def alternate (list1: List[Int], list2: List[Int]) : List[Int] = {
    list1 match {
      case Nil => Nil
      case head :: tail => {
        list2 match {
          case Nil => Nil
          case head :: tail => list1.head :: list2.head :: alternate(list1.tail, list2.tail)
        }
      }
    }
  }
  def insertOrdered(n: Int, ints: List[Int]): List[Int] = {
    ints match {
      case Nil => n :: Nil
      case head :: Nil => {
        if(n < head) {
          n :: head :: Nil
        }
        else {
          head :: n :: Nil
        }
      }
      case head :: x :: tail => {
        if(n < head) {
          n :: head :: x :: tail
        }
        else if(n >= head && n < x){
          head :: n :: x :: tail
        }
        else {
          head :: insertOrdered(n, x :: tail)
        }
      }
    }
  }
}