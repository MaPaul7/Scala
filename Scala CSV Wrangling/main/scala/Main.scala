import  hw.csv._

sealed trait Gender
case class Male() extends Gender
case class Female() extends Gender

case class SSARow(birthYear: Int, name: String, gender: Gender, count: Int)

case class CDCRow(birthYear: Int, maleLifeExpectancy: Int,
  femaleLifeExpectancy: Int)

object Main {

  def readSSARow(row: List[String]): SSARow = {
   SSARow(row(0).toInt, row(1), if(row(2) == "F") Female() else Male(), row(3).toInt)
  }
  def readCDCRow(row: List[String]): CDCRow = {
    CDCRow(row(0).toInt, row(1).toInt, row(2).toInt)
  }
  def yearIs(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear == bound)
  }
  def yearGT(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear > bound)
  }
  def yearLT(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear < bound)
  }
  def onlyName(rows: List[SSARow], name: String): List[SSARow] = {
    rows.filter(row => row.name == name)
  }
  def remove(rows: List[SSARow], name: String): List[SSARow] = rows match {
    case Nil => Nil
    case head :: Nil => {
      if(head.name == name) Nil
      else head :: Nil
    }
    case head :: mid :: tail => {
      if(head.name == name) remove(mid :: tail, name)
      else if(mid.name == name) remove(head :: tail, name)
      else head :: remove(mid :: tail, name)
    }
  }
  /*def getNames(rows: List[SSARow]): List[SSARow] = rows match {
    case Nil => Nil
    case head :: tail => onlyName(rows, head.name) ::: getNames(remove(tail, head.name))
  }*/
  def makeEntries(rows: List[SSARow]): List[(String, Int)] = rows match {
    case Nil => Nil
    case head :: tail => (head.name, count(onlyName(rows, head.name))) :: makeEntries(remove(rows, head.name))
  }
  def mostPopularHelper(rows: List[(String, Int)]): (String, Int) = rows match {
    case Nil => ("None", 0)
    case head :: Nil => head
    case head :: mid :: tail => {
      if(head._2 > mid._2) mostPopularHelper(head :: tail)
      else mostPopularHelper(head :: tail)
    }
  }
  def mostPopular(rows: List[SSARow]): (String, Int) = {
    mostPopularHelper(makeEntries(rows))
  }

  def count(rows: List[SSARow]): Int = rows match {
    case Nil => 0
    case head :: tail => head.count + count(tail)
  }
  def countGirls(rows: List[SSARow]): Int = rows match {
    case Nil => 0
    case head :: tail => {
      if(head.gender == Female()) head.count + countGirls(tail)
      else countGirls(tail)
    }
  }
  def countBoys(rows: List[SSARow]): Int = rows match {
    case Nil => 0
    case head :: tail => {
      if(head.gender == Male()) head.count + countBoys(tail)
      else countBoys(tail)
    }
  }
  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = rows match {
    case Nil => (0, 0)
    case head :: tail => (countGirls(rows), countBoys(rows))
  }
  def getBoyNames(rows: List[SSARow]): Set[String] = rows match {
    case Nil => Set()
    case head :: tail => {
      if(head.gender == Male())  getBoyNames(tail) + head.name
      else getBoyNames(tail)
    }
  }
  def getGirlNames(rows: List[SSARow]): Set[String] = rows match {
    case Nil => Set()
    case head :: tail => {
      if(head.gender == Female()) getGirlNames(tail) + head.name
      else getGirlNames(tail)
    }
  }
  def genderNeutralNames(rows: List[SSARow]): Set[String] = {
    (getGirlNames(rows) & getBoyNames(rows))
  }
  def getYear(lst: List[CDCRow], year: Int): CDCRow = {
    lst.filter(row => row.birthYear == year).head
  }
  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = gender match {
    case Male() => {
      if((currentYear-birthYear) <= getYear(lifeExpectancies, birthYear).maleLifeExpectancy) true
      else false
    }
    case Female() => {
      if((currentYear-birthYear) <= getYear(lifeExpectancies, birthYear).femaleLifeExpectancy) true
      else false
    }
  }
  def estimatePopulation(rows: List[SSARow], year: Int,
    lifeExpectancies: List[CDCRow]): Int = rows match {
    case Nil => 0
    case head :: tail => {
      if(expectedAlive(head.gender, head.birthYear, year, lifeExpectancies))
        head.count + estimatePopulation(tail, year, lifeExpectancies)
      else estimatePopulation(tail, year, lifeExpectancies)
    }
  }
}