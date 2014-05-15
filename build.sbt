name := "SkiCalculator"

version := "1.0"

organization  := "org.bayswater"

scalaVersion := "2.11.0"        

       

libraryDependencies ++= Seq(
  "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.1",
  "org.specs2" % "specs2_2.11" % "2.3.11" % "test"
)                         
     
externalResolvers ++= Seq(
                  "Typesafe"    at "http://repo.typesafe.com/typesafe/releases/", 
                  "sbt-plugin-releases" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/",
                  "Sonatype"  at "http://oss.sonatype.org/content/repositories/releases"
                  )                                
                           
