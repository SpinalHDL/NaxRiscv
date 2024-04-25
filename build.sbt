// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

val spinalVersion = "dev"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.12.18",
      version      := "2.0.0"
    )),
    scalacOptions += s"-Xplugin:${new File(baseDirectory.value + s"/ext/SpinalHDL/idslplugin/target/scala-2.12/spinalhdl-idsl-plugin_2.12-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    scalacOptions += "-language:reflectiveCalls",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17",
      "org.yaml" % "snakeyaml" % "1.8",
      "net.fornwall" % "jelf" % "0.7.0"
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value / "ext/rvls/bindings/jni",
    name := "NaxRiscv"
  ).dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore,spinalHdlLib)
lazy val spinalHdlIdslPlugin = ProjectRef(file("ext/SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("ext/SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("ext/SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("ext/SpinalHDL"), "lib")

fork := true
