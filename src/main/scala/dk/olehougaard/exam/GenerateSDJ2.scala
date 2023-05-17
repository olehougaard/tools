package dk.olehougaard.exam

import java.io.PrintWriter
import scala.util.Using

object GenerateSDJ2 extends App {
  val sdj2 = HtmlTableParser.participantsCSV("SDJ2").filter(_.role == "Student")
  val wednesdays = sdj2.filter(p => Set("333324", "331459", "331906").contains(p.id))

  val daySchedule = DaySchedule(Time(8, 30), Time(16, 10), 20)
    .assignSlot(Time(11, 50), "Break", 2)

  val baseExamSchedule = new ExamScheduleBuilder(sdj2)
    .addDay("Monday 12 June", daySchedule)
    .addDay("Tuesday 13 June", daySchedule.assignSlot(Time(10, 50), "Break", 3))
    .addDay("Wednesday 14 June", DaySchedule(Time(8, 30), Time(9, 30), 20))

  val scheduleBuilder = baseExamSchedule
    .constrain(Set("Monday 12 June", "Tuesday 13 June"), sdj2 -- wednesdays)
    .reserve("Tuesday 13 June", 331882, 331904, 331497, 331337, 333416)
    .reserve("Wednesday 14 June", wednesdays)
    .toExamSchedule

  Using(new PrintWriter(FileIO.toDownloadPath("s2.csv"), "windows-1252"))(scheduleBuilder.print)
}
