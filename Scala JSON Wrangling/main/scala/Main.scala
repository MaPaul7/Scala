import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

  val data: List[Json] = JsonHelper.fromFile("yelp.json")

  def key(json: Json, key: String): Option[Json] = json match {
    case JsonDict(aMap) => aMap.get(JsonString(key))
    case _ => None
  }

  def isFromState(datum: Json, state: String): Boolean = {
    key(datum,"state") match {
      case None => false
      case Some(stateName) => stateName match {
        case JsonString(x) => x == state
        case _ => false
      }
    }
  }
  def hasCategory(datum: Json, category: String): Boolean = {
    key(datum, "categories") match {
      case None => false
      case Some(categoryList) => categoryList match {
        case JsonArray(x) => x.contains(JsonString(category))
        case _ => false
      }
    }
  }
  def isLessThan(datum: Json, rating: Double): Boolean = {
    key(datum, "stars") match {
      case None => false
      case Some(ratingNumber) => ratingNumber match {
        case JsonNumber(x) => (x <= rating)
        case _ => false
      }
    }
  }
  def isGreaterThan(datum: Json, rating: Double): Boolean = {
    key(datum, "stars") match {
      case None => false
      case Some(ratingNumber) => ratingNumber match {
        case JsonNumber(x) => (x >= rating)
        case _ => false
      }
    }
  }

  def fromState(data: List[Json], state: String): List[Json] = {
    data.filter(datum => isFromState(datum, state))
  }
  def ratingLT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => isLessThan(datum, rating))
  }
  def ratingGT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => isGreaterThan(datum, rating))
  }
  def category(data: List[Json], category: String): List[Json] = {
    data.filter(datum => hasCategory(datum, category))
  }

  def groupByState(data: List[Json]): Map[String, List[Json]] = {
    data.groupBy(datum => key(datum, "state") match {
      case Some(JsonString(stateCode)) => stateCode
      case _ => "no state"
    })
  }
  def groupByCategory(data: List[Json]): Map[String, List[Json]] = {
    data.groupBy(datum => datum match {
      case JsonDict(aMap) => aMap.get(JsonString("categories")) match {
        case Some(JsonString(category)) => category
        case _ => "no category"
      }
      case _ => "no category"
    })
  }
  def getBetter(datum: Json, datum2: Json): Option[Json] = {
    key(datum, "stars") match {
      case None => None
      case Some(rating) => rating match {
        case JsonNumber(rating1) => {
          key(datum2, "stars") match {
            case None => None
            case Some(aRating) => aRating match {
              case JsonNumber(rating2) => {
                if(rating1 > rating2) Some(datum)
                else if (rating1 < rating2) Some(datum2)
                else getMoreRatings(datum, datum2)
              }
            case _ => None
            }
          }
        }
        case _ => None
      }
    }
  }
  def getMoreRatings(datum: Json, datum2: Json): Option[Json] = {
    key(datum, "review_count") match {
      case None => None
      case Some(num) => num match {
        case JsonNumber(count1) => {
          key(datum2, "review_count") match {
            case None => None
            case Some(num2) => num2 match {
              case JsonNumber(count2) => {
                if(count1 >= count2) Some(datum)
                else Some(datum2)
              }
            case _ => None
            }
          }
        }
        case _ => None
      }
    }
  }
  def bestPlace(data: List[Json]): Option[Json] = {
    data match {
      case Nil => None
      case head :: tail :: Nil => {
        if(getBetter(head, tail) == Some(head)) Some(head)
        else Some(tail)
      }
      case head :: mid :: tail => {
        if(getBetter(head, mid) == Some(head)) bestPlace(head :: tail)
        else bestPlace(mid :: tail)
      }
      case _ => None
    }
  }
}
