name := "md5-converter"

version := "0.9"

scalaVersion := "2.12.8"

libraryDependencies += "org.tpolecat" %% "atto-core"    % "0.6.5"
libraryDependencies += "org.tpolecat" %% "atto-refined" % "0.6.5"
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

logBuffered in Test := false