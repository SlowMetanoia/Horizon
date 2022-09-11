name := "Horizon"

version := "0.1"

scalaVersion := "2.13.7"

val AkkaVersion = "2.6.19"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
libraryDependencies += "com.flowtick" %% "graphs-graphml" % "0.9.0"
libraryDependencies += "com.flowtick" %% "graphs-core" % "0.9.0"
libraryDependencies += "com.flowtick" %% "xmls" % "0.1.11"
// https://mvnrepository.com/artifact/org.jgrapht/jgrapht
libraryDependencies += "org.jgrapht" % "jgrapht" % "1.5.1" //pomOnly()
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
