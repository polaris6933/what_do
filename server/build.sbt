libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.25"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

cancelable in Global := true
