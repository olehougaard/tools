package dk.olehougaard.exam

import java.io.{BufferedReader, FileReader, PrintWriter}
import scala.collection.mutable
import scala.io.Source
import scala.util.{Random, Try, Using}

case class Participant(id: String, name: String, role: String)

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

  def home = System.getProperty("user.home")
  def toDownloadPath(name: String) = s"$home\\Downloads\\$name"
}

object HtmlTableParser {
  private def parseHTML(html: String): Set[Participant] = {
    val rowRE = raw"<tr>.*?</tr>".r
    val x = rowRE.findAllIn(html).drop(1).flatMap{ row =>
      val cellRE = raw"<td>(.*?)</td><td>.*?</td><td>.*?</td><td>(.*?)</td><td>.*?</td><td>(.*?)</td><td>.*?</td>".r
      cellRE.findFirstMatchIn(row).map { m => Participant(m.group(2), m.group(1), m.group(3))
      }
    }
    x.toSet
  }
  private def parseCSV(lines: Seq[String]): Set[Participant] = {
    val emailRE = raw"(.*)@viauc\.dk".r
    lines
      .map(_.trim)
      .map(_.split(';'))
      .map({case Array(name, email, role) =>
        val id = emailRE.findFirstMatchIn(email).fold(email)(_.group(1))
        Participant(id, name, role)
      })
      .toSet
  }
  def participantsHTML(className: String): Set[Participant] =
    HtmlTableParser.parseHTML(FileIO.loadTextFile(FileIO.toDownloadPath(s"$className.xls")))

  def participantsCSV(className: String): Set[Participant] = {
    val source = Source.fromFile(FileIO.toDownloadPath(s"$className.csv"))
    try {
      parseCSV(source.getLines().toSeq)
    } finally {
      source.close()
    }
  }
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

  def print(pw: PrintWriter): Unit = {
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

  def addDay(day: Day): Unit = {
    days = days :+ day
  }

  def print(pw: PrintWriter): Unit = {
    pw.println()
    days.foreach(_.print(pw))
  }
}

class ExamScheduleBuilder(participants: Set[Participant]) {
  private class DayBuilder(val heading: String, val schedule: DaySchedule, val participants: Seq[Participant] = Seq()) {
    def assign(ps: Seq[Participant]): DayBuilder = {
      if (ps.size > availableSeats) throw new IllegalStateException(heading + " overbooked")
      new DayBuilder(heading, schedule, participants ++ ps)
    }
    def assign(p: Participant): DayBuilder = assign(Seq(p))
    def availableSeats: Int = schedule.free.size - participants.size
    def hasAvailable: Boolean = availableSeats > 0

    def shuffle = new DayBuilder(heading, schedule, Random.shuffle(participants))

    def toDay: Day = {
      val daySchedule = (participants zip schedule.free.toSeq.sorted).foldLeft(schedule) {
        case (s, (p, t)) => s.assignSlot(t, s"${p.id};${p.name}")
      }
      Day(heading, daySchedule)
    }
  }

  private val days = mutable.LinkedHashMap[String, DayBuilder]()
  private val participant = participants.map(p => p.id.toInt -> p).toMap
  private val constraints = mutable.Map[Int, Set[String]]()

  def addDay(heading: String, schedule: DaySchedule): ExamScheduleBuilder = {
    days(heading) = new DayBuilder(heading, schedule)
    this
  }

  def reserve(heading: String, participants: Set[Participant]): ExamScheduleBuilder = reserve(heading, participants.map(_.id.toInt).toSeq:_*)

  def reserve(heading: String, ids: Int*): ExamScheduleBuilder = {
    days(heading) = days(heading).assign(ids.map(participant))
    this
  }

  def constrain(headings: Set[String], participants: Set[Participant]): ExamScheduleBuilder = constrain(headings, participants.map(_.id.toInt).toSeq: _*)

  def constrain(headings: Set[String], ids: Int*): ExamScheduleBuilder = {
    constraints ++= ids.map(id => id -> headings)
    this
  }

  def toExamSchedule: ExamSchedule = {
    val reserved = days.values.map(_.participants).reduce(_ ++ _).distinct
    val plan = days.clone
    val unreserved = participants -- reserved
    Random.shuffle(unreserved).foreach {
      p =>
        val availableDays = constraints.getOrElse(p.id.toInt, plan.keys).map(plan).toSeq
        val bestDay = availableDays.maxBy(_.availableSeats)
        if (bestDay.hasAvailable) plan(bestDay.heading) = bestDay.assign(p)
    }
    plan.values.foreach { d => plan(d.heading) = d.shuffle }
    val assigned = plan.values.map(_.participants).reduce(_ ++ _).distinct
    (participants -- assigned).foreach {
      p =>
        val bestDay = plan.values.filter(_.hasAvailable).maxBy(_.availableSeats)
        plan(bestDay.heading) = bestDay.assign(p)
    }
    val schedule = new ExamSchedule
    plan.values.foreach(d => schedule.addDay(d.shuffle.toDay))
    schedule
  }
}
