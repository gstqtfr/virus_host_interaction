// Name of the project
name := "ISA-MNIST"

// Project version
version := "14-R19"

// Version of Scala used by the project
scalaVersion := "2.13.1"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"
// https://mvnrepository.com/artifact/org.deeplearning4j/deeplearning4j-core
libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "0.9.1"
// https://mvnrepository.com/artifact/com.google.guava/guava
libraryDependencies += "com.google.guava" % "guava" % "29.0-jre"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % "test"  //scalacheck
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0" % "test"     //scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"      //scalatest
resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8", "-feature")

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true

assemblyMergeStrategy in assembly := {
  case m if m.toLowerCase.endsWith("manifest.mf")          => MergeStrategy.discard
  case m if m.toLowerCase.matches("meta-inf.*\\.sf$")      => MergeStrategy.discard
  case "log4j.properties"                                  => MergeStrategy.discard
  case m if m.toLowerCase.startsWith("meta-inf/services/") => MergeStrategy.filterDistinctLines
  case "reference.conf"                                    => MergeStrategy.concat
  case _                                                   => MergeStrategy.first
}