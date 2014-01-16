import AssemblyKeys._

name := "RAbML"

organization := "net.rkmathi"

version := "0.0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.7" % "test"
//  ,"org.scalaz" %% "scalaz-core" % "7.0.5"
//  ,"org.typelevel" %% "scalaz-contrib-210" % "0.1.5"
//  ,"org.typelevel" %% "scalaz-specs2" % "0.1.5" % "test"
  ,"com.typesafe" % "config" % "1.2.0"
//  ,"com.typesafe.akka" %% "akka-actor" % "2.2.3"
)

scalacOptions := Seq(
  "-Xfatal-warnings", "-deprecation", "-feature", "-unchecked"
)

testOptions in (Test, test) += Tests.Argument("console")

seq(assemblySettings: _*)

jarName in assembly <<= (name, version) map {
  (name, version) => name + "-" + version + ".jar"
}

mainClass in assembly := Some("Main")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "application.conf" => MergeStrategy.concat
    case x => old(x)
  }
}

