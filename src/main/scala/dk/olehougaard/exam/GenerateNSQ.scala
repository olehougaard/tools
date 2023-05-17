package dk.olehougaard.exam

import java.io.PrintWriter
import scala.util.Using

object GenerateNSQ extends App {
  val nsq1x = HtmlTableParser.participantsCSV("NSQ1X")
  val nsq1y = HtmlTableParser.participantsCSV("NSQ1Y")
  val nsq = (nsq1x | nsq1y).filter(_.role == "Student")
  val bui = HtmlTableParser.participantsCSV("BUI1").filter(_.role == "Student")

  val daySchedule = DaySchedule(Time(8, 30), Time(16, 10), 20)
    .assignSlot(Time(11, 50), "Break", 2)

  val baseExamSchedule = new ExamScheduleBuilder(nsq)
    .addDay("Thursday 15 June", daySchedule)
    .addDay("Friday 16 June", daySchedule)
    .addDay("Monday 19 June", daySchedule)
    .addDay("Tuesday 20 June", daySchedule)

  val scheduleBuilder = baseExamSchedule
    .reserve("Monday 19 June", 335088)
    .reserve("Friday 16 June", 304829, 304071)
    .constrain(Set("Thursday 15 June", "Friday 16 June"), bui)
    .constrain(Set("Thursday 15 June", "Friday 16 June"), 304506, 329735, 329736, 329852, 335072, 335075, 335084, 335091, 335129, 335139, 335140, 335141, 335142) // Exchange
    .reserve("Thursday 15 June", 329933)
    .toExamSchedule

  Using(new PrintWriter(FileIO.toDownloadPath("nsq.csv"), "windows-1252"))(scheduleBuilder.print)
}
