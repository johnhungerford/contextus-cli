import Dependencies.*

import java.nio.file.{CopyOption, StandardCopyOption}
import scala.collection.Seq
import scala.sys.process.*

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion     := "3.3.3"
ThisBuild / version          := "0.2.7"
ThisBuild / organization     := "io.github.johnhungerford"
ThisBuild / organizationName := "johnhungerford"
ThisBuild / resolvers += "Sonatype OSS Snapshots" at "https://s01.oss.sonatype" +
  ".org/content/repositories/snapshots"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val commonSettings = Seq(
    libraryDependencies ++= zio ++ cli ++ zioNio ++ phobos ++ json ++ sttp,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq("-Xmax-inlines", "128"),
)

lazy val packageSettings = Seq(
    assembly / assemblyMergeStrategy := {
        // Needed for tapir swagger endpoint
        case PathList("META-INF", "maven", "org.webjars", "swagger-ui", "pom.properties") =>
            MergeStrategy.singleOrError
        case PathList("META-INF", "resources", "webjars", "swagger-ui", xs @ _*)          =>
            MergeStrategy.last
        case PathList("META-INF", xs @ _*)                                                =>
            (xs map { _.toLowerCase }) match {
                case "services" :: xs =>
                    MergeStrategy.filterDistinctLines
                case _                => MergeStrategy.discard
            }
        case PathList("reference.conf")                                                   =>
            MergeStrategy.concat
        case PathList("application.conf")                                                 =>
            MergeStrategy.concat
        case x                                                                            =>
            MergeStrategy.last
    },
    assembly / test                  := {},
)

val arch = settingKey[String]("Architecture")

lazy val disablePackaging = Seq()

val packageCli = taskKey[Unit]("Package Contextus CLI")

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging, UniversalPlugin, NativeImagePlugin, BuildInfoPlugin)
  .settings(
      name := "contextus-cli",
      commonSettings,
      packageSettings,
      arch := "arm64",
      buildInfoKeys := Seq[BuildInfoKey](version, arch),
      buildInfoPackage := "contextus.buildinfo",
      Universal / packageName   := "contextus-cli",
      nativeImageOptions ++= Seq(
          // Don't generate JVM version if compilation fails
          "--no-fallback",
          "--enable-url-protocols=http",
          "--install-exit-handlers",
          "--enable-http",
      ),
      nativeImageInstalled      := true,
      Compile / mainClass       := Some("Main"),
      assembly / mainClass      := Some("Main"),
      packageCli := {
          val packageFile = baseDirectory.value / "package"
          val nativeImageFile = nativeImage.value

          arch.value

          java.nio.file.Files.copy(
              nativeImageFile.toPath,
              (packageFile / nativeImageFile.getName).toPath,
              StandardCopyOption.REPLACE_EXISTING,
          )

          val zippedPackageFileName = s"contextus-${version.value}.zip"
          val zippedPackageFile = baseDirectory.value / zippedPackageFileName

          Process("zip" :: "-r" :: zippedPackageFile.getAbsolutePath :: "." :: Nil, Some(packageFile)).!
      }
  )


addCompilerPlugin("com.hmemcpy" %% "zio-clippy" % "0.0.5")
