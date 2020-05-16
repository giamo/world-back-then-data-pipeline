name := "wiki-history"

version := "0.1"

scalaVersion := "2.12.11"

val sparkVersion = "2.4.1"

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "com.databricks" % "spark-xml_2.11" % "0.9.0",
  "info.bliki.wiki" % "bliki-core" % "3.1.0",
  "net.java" % "textile-j" % "2.2",
  "com.github.spyhunter99" % "jdbf" % "2.2.1",
  "org.jsoup" % "jsoup" % "1.13.1",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.1.1" % Test
)