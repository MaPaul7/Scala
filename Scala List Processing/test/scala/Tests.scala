import Lists._

class TestSuite extends org.scalatest.FunSuite {

  test("oddNumbers properly defined") {
    assert(oddNumbers == List(1, 3, 5))
  }
  test("sumDouble") {
    assert(sumDouble(oddNumbers) == 18)
    assert(sumDouble(List()) == 0)
  }
  test("countEvens") {
    assert(countEvens(oddNumbers) == 0)
    assert(countEvens(List(-4, -2, 0, 1, 2)) == 4)
    assert(countEvens(evenNumbers) == 3)
    assert(countEvens(oddsEvens) == 2)
    assert(countEvens(List()) == 0)
  }
  test("removeZeros") {
    assert(removeZeros(zeros) == List(2, 1))
    assert(removeZeros(oddNumbers) == List(1, 3, 5))
    assert(removeZeros(List()) == List())
  }
  test("isAscending") {
    assert(isAscending(oddNumbers) == true)
    assert(isAscending(List(-3, -2, 0, 0, 1)) == true)
    assert(isAscending(zeros) == false)
    assert(isAscending(List(1, 1, 1, 1)) == true)
    assert(isAscending(wrong) == false)
    assert(isAscending(List(1, 2, 3, 5, 6)) == true)
    assert(isAscending(List()) == true)
  }
  test("fromTo") {
    assert(fromTo(1, 4) == List(1, 2, 3))
  }
  test("insertOrdered") {
    assert(insertOrdered(2, oddNumbers) == List(1, 2, 3, 5))
    assert(insertOrdered(6, oddsEvens) == List(1, 2, 3, 4, 6))
    assert(insertOrdered(0, oddNumbers) == List(0, 1, 3, 5))
  }
  test("alternate") {
    assert(alternate(oddNumbers, evenNumbers) == List(1, 2, 3, 4, 5, 6))
    assert(alternate(oddsEvens, zeros) == List(1, 0, 2, 2, 3, 1, 4, 0))
  }
  test("remove") {
    assert(remove(2, List(5, 6, 2, 3, 1)) == List(5, 6, 3, 1))
  }
  test("removeAlternating") {
    assert(removeAlternating(oddsEvens) == List(1, 3))
    assert(removeAlternating(oddNumbers) == List(1, 5))
    assert(removeAlternating(List(1)) == List(1)) 
    assert(removeAlternating(List()) == List())
  }
  test("addSub") {
    assert(addSub(oddsEvens) == -2)
  }
  test("sort") {
   assert(sort(wrong) == List(1, 2, 3, 5, 6))
   assert(sort(zeros) == List(0, 0, 1, 2))
   assert(sort(List(1, 1, 1, 1)) == List(1, 1, 1, 1))
  }
  test("allLT") {
    assert(allLT(4, wrong) == List(2, 3, 1))
  }
  test("allGTE") {
    assert(allGTE(3, wrong) == List(5, 6, 3))
  }
}