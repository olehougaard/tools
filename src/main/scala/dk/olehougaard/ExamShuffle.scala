package dk.olehougaard

import java.io.{BufferedReader, FileReader, PrintWriter}

import scala.util.{Random, Try}

case class Participant(id: String, name: String, role: String)

object ListFactory {
  def generateWhile[T](cond: => Boolean)(generator: => T) = {
    var list = List[T]()
    while(cond) list = generator :: list
    list.reverse
  }
}

object FileIO {
  def loadTextFile(path: String): String = {
    val r = Try(new BufferedReader(new FileReader(path)))
    val s = r
      .map { reader => Stream.iterate(reader.readLine())(_=>reader.readLine())}
      .map (_.takeWhile(_ != null))
      .map (_.mkString)
    r.foreach(_.close())
    s.get
  }
}

object HtmlTableParser {
  def parse(html: String)= {
    val rowRE = raw"<tr>.*?</tr>".r
    val x = rowRE.findAllIn(html).drop(1).flatMap{ row =>
      val cellRE = raw"<td>(.*?)</td><td>.*?</td><td>.*?</td><td>(.*?)</td><td>.*?</td><td>(.*?)</td><td>.*?</td>".r
      cellRE.findFirstMatchIn(row).map { m => Participant(m.group(2), m.group(1), m.group(3))
      }
    }
    x.toSet
  }
  def participants(className: String) =
    HtmlTableParser.parse(FileIO.loadTextFile(s"C:\\Users\\Ole\\Downloads\\$className.xls"))
  def students(className: String) = participants(className).filter(_.role == "Studerende")
}

object ExamShuffle extends App {
  val swa1x = HtmlTableParser.students("SWA1X-A18")
  val swa1y = HtmlTableParser.students("SWA1Y-A18")
  val pcl1 = HtmlTableParser.students("PCL1-A18")
  val dnp2 = HtmlTableParser.students("DNP2-A18")
  val swa = swa1x | swa1y
  val students = swa.map(p => p.id.toInt -> p).toMap
  val mojtaba = students(240354)
  val spencer = students(245487) //not 8
  val lea = students(239835)
  val bashar = students(246678)
  val venelina = students(239907)
  val mihai = students(245485)
  val palle = students(231219)
  val request7 = Set(mojtaba, lea, bashar)
  val request11 = Set(venelina, mihai, palle)
  val requestNot8 = Set(spencer)


  var monday = request7
  var tuesday = Set[Participant]()
  var thursday = Set[Participant]()
  var friday = request11

  var remaining = swa -- monday -- tuesday -- thursday -- friday

  val availableMonday = remaining -- request11 -- dnp2
  monday ++= Random.shuffle((availableMonday & pcl1).toSeq).take(21 - monday.size)
  monday ++= Random.shuffle((availableMonday -- monday).toSeq).take(21 - monday.size)
  remaining --= monday

  val availableTuesday = remaining -- requestNot8 -- pcl1 -- dnp2
  tuesday ++= Random.shuffle(availableTuesday.toSeq).take(20 - tuesday.size)
  remaining --= tuesday

  val availableThursday = remaining -- pcl1
  thursday ++= Random.shuffle(availableThursday.toSeq).take(20 - thursday.size)
  remaining --= thursday

  friday ++= remaining

  def printDay(pw: PrintWriter, heading: String, participants: Set[Participant]) {
    val hours = Seq("08", "09", "10", "11", "12", "13", "14", "15", "16")
    val minutes = Seq("00", "20", "40")
    val slots = hours.flatMap(h => minutes.map(m => s"$h:$m"))
    val breaks = Map("10:00" -> ("break", Set("10:00")), "12:00" -> ("lunch", Set("12:00", "12:20", "12:40")))
    val breakSlots = breaks.values.flatMap(_._2).toSet
    val schedule = slots.filterNot(breakSlots.apply) zip Random.shuffle(participants.toSeq)
    val scheduleLines = schedule.map{case (t, Participant(id, name, _)) => s";$t;$id;$name"}
    val breakLines = breaks.map { case (t, (txt, _)) => s";$t;$txt"}
    pw.print(";")
    pw.println(heading)
    pw.println()
    pw.println(";Time;Student number; Name")
    (scheduleLines ++ breakLines).sorted.foreach(pw.println)
    pw.println()
    pw.println()
  }
  val pw = new PrintWriter(raw"C:\Users\Ole\Downloads\swa.csv", "windows-1252")
  try {
    pw.println()
    printDay(pw, "Monday 7 January", monday)
    printDay(pw, "Tuesday 8 January", tuesday)
    printDay(pw, "Thursday 10 January", thursday)
    printDay(pw, "Friday 11 January", friday)
  } finally {
    pw.close()
  }
}
