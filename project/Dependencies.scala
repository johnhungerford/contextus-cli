import sbt._

object Dependencies {
	val zioVersion = "2.1-RC1"
	val phobosVersion = "0.21.0"
	val circeVersion = "0.14.6"
	val sttpVersion = "3.9.3"
	val zioCliVersion = "0.5.0"
	val zioNioVersion = "2.0.2"

	val zio = Seq(
		"dev.zio" %% "zio" % "2.0.21",
		"dev.zio" %% "zio-streams" % zioVersion,
		"dev.zio" %% "zio-test" % zioVersion % Test,
		"dev.zio" %% "zio-test-sbt" % zioVersion % Test,
		"dev.zio" %% "zio-test-magnolia" % zioVersion % Test
	)

	val zioNio = Seq(
		"dev.zio" %% "zio-nio" % zioNioVersion,
	)

	val cli = Seq(
		"dev.zio" %% "zio-cli" % zioCliVersion,
	)

	val phobos = Seq(
		"ru.tinkoff" %% "phobos-core" % phobosVersion,
	)

	val json = Seq(
		"io.circe" %% "circe-core"       % circeVersion,
		"io.circe" %% "circe-generic"    % circeVersion,
		"io.circe" %% "circe-parser"     % circeVersion,
	)

	val sttp = Seq(
		"com.softwaremill.sttp.client3" %% "core" % sttpVersion,
		"com.softwaremill.sttp.client3" %% "zio" % sttpVersion,
	)
}
