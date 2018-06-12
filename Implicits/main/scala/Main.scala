import java.nio.file._
import java.io.File
import java.nio.file.Paths
import java.time.LocalDate

object PathImplicits {
  implicit class RichString(s: String) { 
    def / (s2: String): Path = {
      Paths.get(s,s2)
    }
    def / (p: Path): Path = {
      Paths.get(s).resolve(p)
    }
  }
  implicit class RichPath(p: Path) {
    def / (s: String): Path = {
      p.resolve(s)
    }
    def / (p2: Path): Path = {
      p.resolve(p2)
    }
    def write(s: String): Path = {
      if(!Files.exists(p)) {
        Files.createFile(Files.write(p, s.getBytes()))
      }
      else {
        Files.write(p, s.getBytes())
      }
    }
    def read(): String = {
      (Files.readAllBytes(p).map(_.toChar)).mkString 
    }
    def append(s: String): Path = {
      if(!Files.exists(p)) {
        write(s)
      }
      else {
        Files.write(p, ((Files.readAllBytes(p).map(_.toChar)).mkString).getBytes() ++  s.getBytes())
      }
    }
  }
}

object TimeImplicits {

  implicit class RichInt(i: Int) {
    def days: LocalDate = {
      LocalDate.of(2000, 1, 1).plusDays(i)
    }
    def months: LocalDate = {
      LocalDate.of(2000, 1, 1).plusMonths(i)
    }
    def years: LocalDate = {
      LocalDate.of(2000, 1, 1).plusYears(i)
    }
    def jan: LocalDate = {
      LocalDate.of(2018, 1, i)
    }
    def jan(i2: Int): LocalDate = {
      LocalDate.of(i2, 1, i)
    }
    def feb: LocalDate = {
      LocalDate.of(2018, 2, i)
    }
    def feb(i2: Int): LocalDate = {
      LocalDate.of(i2, 2, i)
    }
    def mar: LocalDate = {
      LocalDate.of(2018, 3, i)
    }
    def mar(i2: Int): LocalDate = {
      LocalDate.of(i2, 3, i)
    }
    def apr: LocalDate = {
      LocalDate.of(2018, 4, i)
    }
    def apr(i2: Int): LocalDate = {
      LocalDate.of(i2, 4, i)
    }
    def may: LocalDate = {
      LocalDate.of(2018, 5, i)
    }
    def may(i2: Int): LocalDate = {
      LocalDate.of(i2, 5, i)
    }
    def jun: LocalDate = {
      LocalDate.of(2018, 6, i)
    }
    def jun(i2: Int): LocalDate = {
      LocalDate.of(i2, 6, i)
    }
    def jul: LocalDate = {
      LocalDate.of(2018, 7, i)
    }
    def jul(i2: Int): LocalDate = {
      LocalDate.of(i2, 7, i)
    }
    def aug: LocalDate = {
      LocalDate.of(2018, 8, i)
    }
    def aug(i2: Int): LocalDate = {
      LocalDate.of(i2, 8, i)
    }
    def sep: LocalDate = {
      LocalDate.of(2018, 9, i)
    }
    def sep(i2: Int): LocalDate = {
      LocalDate.of(i2, 9, i)
    }
    def oct: LocalDate = {
      LocalDate.of(2018, 10, i)
    }
    def oct(i2: Int): LocalDate = {
      LocalDate.of(i2, 10, i)
    }
    def nov: LocalDate = {
      LocalDate.of(2018, 11, i)
    }
    def nov(i2: Int): LocalDate = {
      LocalDate.of(i2, 11, i)
    }
    def dec: LocalDate = {
      LocalDate.of(2018, 12, i)
    }
    def dec(i2: Int): LocalDate = {
      LocalDate.of(i2, 12, i)
    }
  }
  implicit class RichDate(l: LocalDate) {
    def + (l1: LocalDate): LocalDate = {
      if(l1.getMonthValue()!=1 && l1.getDayOfMonth() == 1) {
        l.plusMonths(l1.getMonthValue()-1)
      }
      else if(l1.getDayOfYear!=1) {
        l.plusDays(l1.getDayOfYear()-1)
      }
      else {
        l.plusYears(l1.getYear()-2000)
      }
    }
  }
}