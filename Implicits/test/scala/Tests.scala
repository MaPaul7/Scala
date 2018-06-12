class Tests extends org.scalatest.FunSuite {

  import PathImplicits._
  import TimeImplicits._

  import java.nio.file._
  import java.io.File
  import java.nio.file.Paths
  import java.time.LocalDate

  test("test write read append") {
    val p = Paths.get("greeting.txt")
    p.write("Hello, world")
    assert(p.read() == "Hello, world")
    assert(p.append(" Hello again").read() == "Hello, world Hello again")
    assert(p.write("Hi").read() == "Hi")

  }
  test("test jan feb mar") {
    assert(15.jan == LocalDate.of(2018, 1, 15))
    assert(4.feb(1999) == LocalDate.of(1999, 2, 4))
    assert(21.mar == LocalDate.of(2018, 3, 21))
  }
  test("test apr may jun") {
    assert(11.apr == LocalDate.of(2018, 4, 11))
    assert(13.may(2001) == LocalDate.of(2001, 5, 13))
    assert(21.jun == LocalDate.of(2018, 6, 21))
  }
  test("date") {
    assert((31 jan 2016) + 5.days == LocalDate.of(2016, 2, 5))
    assert((31 dec 2016) + 0.days == LocalDate.of(2016, 12, 31))
    assert((10 jan 2018) + 10.days == LocalDate.of(2018, 1, 20))
    assert((3 oct 2018) + 5.months == LocalDate.of(2019, 3, 3))
    assert((1 jan 2017) + 2.years == LocalDate.of(2019, 1, 1))
    assert((1 jan 2017) + 32.days == LocalDate.of(2017, 2, 2))
  }
}