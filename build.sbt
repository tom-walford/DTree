name := "DTree"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.0"

crossPaths := false

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked"
)