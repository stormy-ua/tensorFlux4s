scalaVersion in Global := "2.11.8"

lazy val dsl_ = Project(id = "dsl", base = file("dsl"))
lazy val binding_nd4j = project.dependsOn(dsl_ % "test->test;compile->compile")
lazy val core = project.dependsOn(dsl_ % "test->test;compile->compile", binding_nd4j % "test->test")
lazy val examples = project.dependsOn(core % "test->test;compile->compile",
  binding_nd4j % "test->test;compile->compile")

lazy val tensorFlux4s  = project.in(file(".")).aggregate(dsl_, core, binding_nd4j, examples)

