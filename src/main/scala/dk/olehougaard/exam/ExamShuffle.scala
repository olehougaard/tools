package dk.olehougaard.exam

import java.io.{BufferedReader, FileReader, PrintWriter}

import scala.collection.mutable
import scala.util.{Random, Try}

case class Participant(id: String, name: String, role: String)

object ListFactory {
  def generateWhile[T](cond: => Boolean)(generator: => T): List[T] = {
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
  def parse(html: String): Set[Participant] = {
    val rowRE = raw"<tr>.*?</tr>".r
    val x = rowRE.findAllIn(html).drop(1).flatMap{ row =>
      val cellRE = raw"<td>(.*?)</td><td>.*?</td><td>.*?</td><td>(.*?)</td><td>.*?</td><td>(.*?)</td><td>.*?</td>".r
      cellRE.findFirstMatchIn(row).map { m => Participant(m.group(2), m.group(1), m.group(3))
      }
    }
    x.toSet
  }
  def participants(className: String): Set[Participant] =
    HtmlTableParser.parse(FileIO.loadTextFile(s"C:\\Users\\Ole\\Downloads\\$className.xls"))
  def students(className: String): Set[Participant] = participants(className).filter(_.role == "Studerende")
}

class ParticipantMap(all: Set[Participant]) {
  private val participantMap = all.map(p => p.id.toInt -> p).toMap
  def lookup(ids: Int*): Seq[Participant] = ids.map(participantMap)
}

class Time private(val hours: Int, val minutes: Int) extends Ordered[Time] {
  override def compare(that: Time): Int = if (hours != that.hours) hours - that.hours else minutes - that.minutes
  def forward(hours: Int, minutes: Int): Time = Time(this.hours + hours, this.minutes + minutes)
  def forward(minutes: Int): Time = forward(0, minutes)
  override def toString: String = "%02d:%02d".format(hours, minutes)
  override def equals(o: Any): Boolean = o match {
    case t: Time => (hours, minutes) == (t.hours, t.minutes)
    case _ => false
  }
  override def hashCode: Int = hours * 60 + minutes
}

object Time {
  def apply(hours: Int, minutes: Int = 0) = new Time(hours + minutes / 60, minutes % 60)
}

class DaySchedule private(slots: Stream[Time], freeSlots: Set[Time], scheduled: Map[Time, String]) {
  def assignSlot(t: Time, o: Any, size: Int = 1): DaySchedule = {
    val i = slots.indexOf(t)
    val requestedSlots = (i until (i + size)).map(slots)
    if (!requestedSlots.forall(freeSlots)) throw new IllegalStateException("Overbooked")
    new DaySchedule(slots, freeSlots -- requestedSlots, scheduled + (t -> o.toString))
  }

  def schedule: Map[Time, String] = scheduled
  def free: Set[Time] = freeSlots

  def print(pw: PrintWriter) {
    slots.foreach {
      case t if scheduled.isDefinedAt(t) => pw.println(s";$t;${scheduled(t)}")
      case _ =>
    }
  }
}

object DaySchedule {
  def apply(start: Time, end: Time, minutes: Int): DaySchedule = {
    val slots = Stream.iterate(start)(_.forward(minutes)).takeWhile(_ < end)
    val freeSlots = slots.toSet
    val scheduled = Map[Time, String]()
    new DaySchedule(slots, freeSlots, scheduled)
  }
}

case class Day(heading: String, schedule: DaySchedule) {
  def print(pw: PrintWriter): Unit = {
    pw.println(s";$heading")
    pw.println()
    pw.println(";Time;Student number; Name")
    schedule.print(pw)
    pw.println()
    pw.println()
  }
}

class ExamSchedule {
  private var days = Seq[Day]()

  def addDay(day: Day) {
    days = days :+ day
  }

  def print(pw: PrintWriter): Unit = {
    pw.println()
    days.foreach(_.print(pw))
  }
}

class ExamScheduleBuilder(participants: Set[Participant]) {
  private class DayBuilder(val heading: String, val schedule: DaySchedule, val participants: Set[Participant] = Set()) {
    def assign(ps: Seq[Participant]): DayBuilder = {
      if (ps.size > availableSeats) throw new IllegalStateException("Overbooked")
      new DayBuilder(heading, schedule, participants ++ ps)
    }
    def assign(p: Participant): DayBuilder = assign(Seq(p))
    def availableSeats: Int = schedule.free.size - participants.size
    def hasAvailable: Boolean = availableSeats > 0

    def toDay: Day = {
      val plan = Random.shuffle(participants.toSeq)
      val daySchedule = (plan zip schedule.free.toSeq.sorted).foldLeft(schedule) {
        case (s, (p, t)) => s.assignSlot(t, s"${p.id};${p.name}")
      }
      Day(heading, daySchedule)
    }
  }

  private val days = mutable.LinkedHashMap[String, DayBuilder]()
  private val participant = participants.map(p => p.id.toInt -> p).toMap
  private val constraints = mutable.Map[Int, Set[String]]()

  def addDay(heading: String, schedule: DaySchedule) {
    days(heading) = new DayBuilder(heading, schedule)
  }

  def reserve(heading: String, ids: Int*) {
    days(heading) = days(heading).assign(ids.map(participant))
  }

  def constrain(headings: Set[String], ids: Int*): Unit = {
    constraints ++= ids.map(id => id -> headings)
  }

  def unconstrained: Set[Participant] =
    participants --
      constraints.keys.map(participant) --
      days.values.foldLeft(Set[Participant]())(_ | _.participants)

  def toExamSchedule: ExamSchedule = {
    val reserved = days.values.map(_.participants).reduce(_ | _)
    val plan = days.clone
    (participants -- reserved).foreach {
      p =>
        val availableDays = constraints.getOrElse(p.id.toInt, plan.keys).map(plan).toSeq
        val bestDay = availableDays.maxBy(_.availableSeats)
        plan(bestDay.heading) = bestDay.assign(p)
    }
    val schedule = new ExamSchedule
    plan.values.foreach(d => schedule.addDay(d.toDay))
    schedule
  }
}

object ExamShuffle extends App {
  val swa1x = HtmlTableParser.students("IT-SWA1X-A19")
  val swa1y = HtmlTableParser.students("IT-SWA1Y-A19")
  val swa = swa1x | swa1y
  val students = swa.map(p => p.id.toInt -> p).toMap

  val headings = Seq("Monday 6 January", "Tuesday 7 January", "Wednesday 8 January", "Thursday 9 January", "Friday 10 January")
  val Seq(_6, _7, _8, _9, _10) = headings
  val xExamDays = Set(_6, _7)
  val yExamDays = Set(_8, _9, _10)

  val skeleton = DaySchedule(Time(8), Time(16), 20).assignSlot(Time(10), "Break").assignSlot(Time(12), "Lunch", 2)
  val shortSkeleton = DaySchedule(Time(9), Time(16), 20).assignSlot(Time(12), "Lunch", 2)
  val builder = new ExamScheduleBuilder(swa)
  xExamDays.foreach(builder.addDay(_, skeleton))
  yExamDays.foreach(builder.addDay(_, shortSkeleton))

  builder.reserve(_6, 253762, 260080, 253659, 254175, 253736, 166843, 253737, 242846, 260067, 166894, 261306, 261824, 291156)
  builder.reserve(_7, 253899, 254162)
  builder.reserve(_8, 239857, 240246, 253931, 253911, 253896, 253640, 253992, 253785, 254006, 240329)
  builder.reserve(_9, 253979)
  builder.reserve(_10, 245496, 254134, 267311, 253736, 253668, 253651)

  builder.constrain(Set(_6, _7), 220805)
  builder.constrain(Set(_7, _8), 253810)
  builder.constrain(Set(_8, _9), 254135, 253760, 253765, 254163)
  builder.constrain(Set(_6, _9), 253746, 253739)
  builder.constrain(Set(_9, _10), 253904)

  builder.constrain(xExamDays, (swa1x & builder.unconstrained).toSeq.map(_.id.toInt):_*)
  builder.constrain(yExamDays, (swa1y & builder.unconstrained).toSeq.map(_.id.toInt):_*)

  val schedule = builder.toExamSchedule
  val pw = new PrintWriter(raw"C:\Users\Ole\Downloads\swa.csv", "windows-1252")
  try {
    schedule.print(pw)
  } finally {
    pw.close()
  }
}
