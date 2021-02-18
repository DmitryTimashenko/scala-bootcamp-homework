import sbt.Keys._
import sbt._

object BulkySourcesPlugin extends AutoPlugin {

  import autoImport._

  object autoImport {
    lazy val bulkyThresholdInLines= settingKey[Int]("Threshold of the bulky lines")
    lazy val bulkySources = taskKey[Seq[(Int, File)]]("List of Bulky sources with the number of lines")
  }

  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,
  )

  lazy val helloTask = taskKey[Unit]("Prints Hello world.")

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkySources := getBulkyLines((Compile / sources).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := getBulkyLines((Test / sources).value, bulkyThresholdInLines.value))

  private def getBulkyLines(files: Seq[File], threshold: Int): Seq[(Int, sbt.File)] = {
    files.map { file => (sbt.IO.readLines(file).length, file) }
      .filter { case (count, _) => count > threshold }
      .sortBy { case (count, _) => count }
      .reverse
  }
}
