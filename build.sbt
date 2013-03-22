name := "Adjoint"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "net.sourceforge.parallelcolt" % "parallelcolt" % "0.10.0"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.1"

libraryDependencies += "org.scalanlp" %% "breeze-viz" % "0.1"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))