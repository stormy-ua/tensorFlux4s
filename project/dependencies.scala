import sbt._

object dependencies {

  def _test     (module: ModuleID): ModuleID = module % "test"
  def _provided (module: ModuleID): ModuleID = module % "provided"

  object Versions {
    val nd4j = "0.7.2"
    val cats = "1.0.0-MF"
  }


  object nd4j {
    val group       = "org.nd4j"
    
    val nd4s = group %% "nd4s"  % Versions.nd4j
    val native = group % "nd4j-native-platform"  % Versions.nd4j

  }

  object cats {
    val group = "org.typelevel"

    val core = group %% "cats-core" % Versions.cats
    val free = group %% "cats-free" % Versions.cats
  }

}
