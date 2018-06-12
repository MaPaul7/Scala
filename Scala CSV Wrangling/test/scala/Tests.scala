import hw.csv._
import Main._

class TestSuite extends org.scalatest.FunSuite {

  val SSARowList = List(SSARow(1998, "Max", Male(), 80), SSARow(1997, "Nathan", Male(), 102), 
  SSARow(1938, "Susan", Female(), 90), SSARow(1969, "Reid", Male(), 31))

  val SSARowList2 = List(SSARow(1998, "Jamie", Male(), 80), SSARow(1997, "Alex", Male(), 102), 
  SSARow(1938, "Jamie", Female(), 90), SSARow(1998, "Max", Male(), 31),
  SSARow(1969, "Alex", Female(), 31), SSARow(1998, "Hannah", Female(), 31))

  val SSARowList3 = List(SSARow(1998, "Max", Male(), 80), SSARow(1997, "Nathan", Male(), 102), 
  SSARow(1999, "Hannah", Female(), 90), SSARow(1990, "Dude", Male(), 31))

  val CDCList = List(CDCRow(2010,76,81), CDCRow(2009,75,80), CDCRow(2008,75,80), CDCRow(2007,75,80), 
  CDCRow(2006,75,80), CDCRow(2005,75,80), CDCRow(2004,75,80), CDCRow(2003,75,80), CDCRow(2002,75,80),
  CDCRow(2001,74,80), CDCRow(2000,74,80), CDCRow(1999,74,79), CDCRow(1998,74,80), CDCRow(1997,74,79),
  CDCRow(1996,73,79), CDCRow(1995,73,79), CDCRow(1994,72,79), CDCRow(1993,72,79),
  CDCRow(1992,72,79), CDCRow(1991,72,79), CDCRow(1990,72,79))
  
  test("test readSSARow"){
    assert(readSSARow(List("1998", "Max", "M", "80")) == SSARow(1998, "Max", Male(), 80))
  }
  test("test makeEntries") {
    assert(makeEntries(SSARowList2) == List(("Jamie", 170), ("Alex", 133), ("Max", 31), ("Hannah", 31)))
  }
  test("test mostPopular"){
    assert(mostPopular(SSARowList2) == ("Jamie", 170))
  }
  test("test readCDCRow"){
    assert(readCDCRow(List("1998", "56", "64")) == CDCRow(1998, 56, 64))
  }
  test("test yearIs"){
    assert(yearIs(SSARowList, 1998) == List(SSARow(1998, "Max", Male(), 80)))
  }
  test("test yearGT"){
    assert(yearGT(SSARowList, 1990) == List(SSARow(1998, "Max", Male(), 80), SSARow(1997, "Nathan", Male(), 102)))
  }
  test("test yearLT"){
    assert(yearLT(SSARowList, 1990) == List(SSARow(1938, "Susan", Female(), 90), SSARow(1969, "Reid", Male(), 31)))
  }
  test("test onlyName"){
    assert(onlyName(SSARowList, "Max") == List(SSARow(1998, "Max", Male(), 80)))
  }
  test("test count"){
    assert(count(SSARowList) == 303)
  }
  test("test countGirlsAndBoys"){
    assert(countGirlsAndBoys(SSARowList) == (90, 213))
  }
  test("test genderNeutralNames"){
    assert(genderNeutralNames(SSARowList2).contains("Jamie"))
    assert(!genderNeutralNames(SSARowList2).contains("Hannah"))
    assert(genderNeutralNames(SSARowList2).contains("Alex"))
    assert(!genderNeutralNames(SSARowList2).contains("Max"))
  }
  test("test expectedAlive"){
    assert(expectedAlive(Male(), 1998, 2020, CDCList) == true)
    assert(expectedAlive(Male(), 1990, 2062, CDCList) == true)
    assert(expectedAlive(Male(), 1990, 2063, CDCList) == false)
    assert(expectedAlive(Female(), 1998, 2020, CDCList) == true)
    assert(expectedAlive(Female(), 1990, 2069, CDCList) == true)
    assert(expectedAlive(Female(), 1990, 2070, CDCList) == false)
  }
  test("estimate population"){
    assert(estimatePopulation(SSARowList3, 2072, CDCList) == 170)
  }
}