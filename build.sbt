name := "MatrixMeta"

version := "0.1.0-SNAPSHOT"

organization      := "com.kabouterlabs"

scalaVersion := "2.11.8"

publishMavenStyle := true


libraryDependencies += "org.spire-math" %% "spire" % "0.7.4"

libraryDependencies  ++= Seq(
  // Last snapshot
  //"org.scalanlp" %% "breeze" % "latest.integration",
  "org.scalanlp" %% "breeze" % "0.11",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.11"

  // The visualization library is distributed separately as well.
  // It depends on LGPL code.
  //"org.scalanlp" %% "breeze-viz" % "0.12"
)
// https://mvnrepository.com/artifact/org.jblas/jblas
libraryDependencies += "org.jblas" % "jblas" % "1.2.4"
// https://mvnrepository.com/artifact/org.apache.commons/commons-math3
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
// https://mvnrepository.com/artifact/org.armadillojava/armadillojava
libraryDependencies += "org.armadillojava" % "armadillojava" % "4.000.0"

libraryDependencies += "jeigen" % "jeigen" % "1.2" from "https://github.com/hughperkins/jeigen/releases/download/v1.2/Jeigen-onefat.jar"
libraryDependencies += "jeigen" % "jna" % "4.1.0" from "https://github.com/hughperkins/jeigen/releases/download/v1.2/jna-4.1.0.jar"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"



