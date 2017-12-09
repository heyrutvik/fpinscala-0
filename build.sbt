name := "fpInScala"
version := "0.0.1"
scalaVersion := "2.12.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false
