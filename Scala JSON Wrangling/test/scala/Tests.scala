import hw.json._
import Wrangling._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  test("Empty test works") {
    assert(true)
  }
  val myJson: Json = JsonDict(Map(JsonString("state") -> JsonString("MA")))
  val moreJson: Json = JsonDict(Map(JsonString("state") -> JsonString("NJ")))

  val jsonListA: List[Json] = List(myJson, moreJson)

  val thisJson: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(4)))
  val thatJson: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(2)))

  val jsonListB: List[Json] = List(thisJson, thatJson)

  val categoryJson: Json = JsonDict(Map(JsonString("categories") -> JsonArray(List(JsonString("Food"), JsonString("Soul")))))
  val thiscategoryJson: Json = JsonDict(Map(JsonString("categories") -> JsonArray(List(JsonString("stuff"), JsonString("stuf")))))
  
  val jsonListC: List[Json] = List(categoryJson, thiscategoryJson)

  test("isFromState") {
    assert(isFromState(myJson, "MA"))
    assert(fromState(jsonListA, "NJ") == List(moreJson))
    assert(fromState(jsonListA, "ME") == Nil)
  }
  
  test("ratingLT") {
    assert(isLessThan(thisJson, 5))
    assert(ratingLT(jsonListB, 3) == List(thatJson))
    assert(ratingLT(jsonListB, 0) == Nil)
  }
  test("ratingGT") {
    assert(isGreaterThan(thisJson, 3))
    assert(ratingGT(jsonListB, 3) == List(thisJson))
  }
  test("category") {
    assert(hasCategory(categoryJson, "Food"))
    assert(category(jsonListC, "Food") == List(categoryJson))
  }
  test("getBetter") {
    assert(getBetter(thisJson, thatJson) == Some(thisJson))
  }
  test("bestPlace") {
    assert(bestPlace(jsonListB) == Some(thisJson))
  }
}
