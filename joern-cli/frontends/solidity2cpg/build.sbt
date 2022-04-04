name := "solidity2cpg"

scalaVersion := "2.13.8"
crossScalaVersions := Seq("2.13.8", "3.1.1")

dependsOn(Projects.dataflowengineoss, Projects.x2cpg % "compile->compile;test->test")

libraryDependencies ++= Seq(
  "io.shiftleft"            %% "codepropertygraph" % Versions.cpg,
  "io.shiftleft"            %% "semanticcpg"       % Versions.cpg,
  "org.apache.logging.log4j" % "log4j-slf4j-impl"  % Versions.log4j     % Runtime,
  "io.spray"                %% "spray-json"        % "1.3.6",
  "io.shiftleft"            %% "semanticcpg"       % Versions.cpg       % Test classifier "tests",
  "org.scalatest"           %% "scalatest"         % Versions.scalatest % Test
)

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

enablePlugins(JavaAppPackaging)
trapExit    := false
Test / fork := true