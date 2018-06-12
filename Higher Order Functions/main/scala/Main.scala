object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = {//???
    lst1 match {
      case Nil => Nil
      case head :: tail => {
        lst2 match {
          case Nil => Nil
          case head :: tail => f(lst1.head, lst2.head) :: map2(f: (A, B) => C, lst1.tail, lst2.tail)
        }
      }
    }
  }

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = {
    lst1 match {
      case Nil => Nil
      case head :: tail => {
        lst2 match {
          case Nil => Nil
          case head :: tail => (lst1.head, lst2.head) :: zip[A, B](lst1.tail, lst2.tail)
        }
      }
    } 
  }
  def listConcat[A](lst: List[A], lst2: List[A]): List[A] = {
    lst match {
      case Nil => {
        lst2 match {
          case Nil => Nil
          case head :: tail => head :: listConcat(lst, lst2.tail)
        }
      }
      case head :: tail => head :: listConcat(lst.tail, lst2)
    }
  }
  def flatten[A](lst: List[List[A]]): List[A] = {
    lst match {
      case Nil => Nil
      case head :: tail => listConcat(head, flatten(tail))
    }
  }

  def flatten3[A](lst: List[List[List[A]]]): List[A] = {
    lst match {
      case Nil => Nil
      case head :: tail => listConcat(flatten(head), flatten3(tail))
    }
  }

  def buildList[A](length: Int, f: Int => A): List[A] = {
    length match {
      case 0 => Nil
      case n => listConcat(buildList(n-1, f: Int=> A), List(f(n-1)))
    }
  }
  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = {
    lst match {
      case Nil => Nil
      case head :: tail => listConcat(f(head), mapList(tail, f: A => List[B]))
    }
  }

  def buildListTrue[A](f: A=> Boolean, lst: List[A]): List[A] = {
    lst match {
      case Nil => Nil
      case head :: tail => {
        if(f(head) == true) {
          head :: buildListTrue(f: A=> Boolean, tail)
        }
        else {
          buildListTrue(f: A=> Boolean, tail)
        }
      }
    }
  }
  def buildListFalse[A](f: A=> Boolean, lst: List[A]): List[A] = {
    lst match {
      case Nil => Nil
      case head :: tail => {
        if(f(head) == false) {
          head :: buildListFalse(f: A=> Boolean, tail)
        }
        else {
          buildListFalse(f: A=> Boolean, tail)
        }
      }
    }
  }
  def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = {
    lst match {
      case Nil => (Nil, Nil)
      case head :: tail => (buildListTrue(f, lst), buildListFalse(f, lst))
    }
  }

  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = {
    alist1 match {
      case Nil => {
        alist2 match {
          case Nil => Nil
          case head :: tail => head :: merge[A](lessThan: (A, A) => Boolean, alist1, alist2.tail)
        }
      }
      case head :: tail => {
        alist2 match {
          case Nil => Nil
          case head :: tail => {
            if(lessThan(alist1.head, alist2.head)) {
              alist1.head :: merge[A](lessThan: (A, A) => Boolean, alist1.tail, alist2)
            }
            else {
              alist2.head :: merge[A](lessThan: (A, A) => Boolean, alist1, alist2.tail)
            }
          }
        }
      }
    }
  }
  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = {
    alist match {
      case Nil => Nil
      case head :: x :: tail => {
        if(head == x) {
          head :: sort[A](lessThan: (A, A) => Boolean, x :: tail)
        }
        else if(!lessThan(head, x)) {
          sort[A](lessThan: (A, A) => Boolean, x :: head :: tail)
        }
        else {
          head :: sort[A](lessThan: (A, A) => Boolean, x :: tail)
        }
      }
      case head :: tail => head :: sort[A](lessThan: (A, A) => Boolean, tail)
    }
  }
}