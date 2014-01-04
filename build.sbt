
scalaVersion := "2.10.3"

libraryDependencies ++=
  Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.5",
    "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.5",
    "org.scalautils" % "scalautils_2.10" % "2.0",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    "org.scalacheck" % "scalacheck_2.10" % "1.10.1" % "test"
  )

