import com.c12e.sbt.Build._


libraryDependencies ++=
  Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.8",
    "org.scalaz" %% "scalaz-concurrent" % "7.1.8",
    "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test")
